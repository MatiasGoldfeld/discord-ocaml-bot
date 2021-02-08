open Core
open Async
open Disml
open Models

let prefix_pattern = Pcre.regexp "^!ocaml(?:\\s|```)"
let command_pattern = Pcre.regexp
    "^!ocaml(?:\\s+|(?=```))(`{0,3})(?:(?<=```)ocaml\n)?([\\s\\S]+)\\1"

(* This is here for security purposes. It is probably not enough, but the
   intention is to prevent Discord users from accessing your system. *)
let wipe_sys = 
  "module Sys = struct end \
   module Stdlib = struct include Stdlib module Sys = struct end end;;"

(* Warning: If ocaml is not called directly, then the system will open ocaml
   through the shell, which will likely cause this program to be unable to 
   properly kill the ocaml instance, as it does not respond to SIGHUP. *)
let run_ocaml = Process.create ~prog:"/usr/bin/ocaml"
    ~args:["-color"; "never"; "-stdin"; "-noinit"]

let sanitize = Pcre.replace ~pat:"``" ~templ:"`\u{200B}`"

let interface (code : string) : string Deferred.t =
  let%bind path, fd = Unix.mkstemp "/tmp/discord-ocaml-bot" in
  let writer = Writer.create fd in
  Writer.write writer code;
  let%bind () = Writer.close writer in
  let out = Process.run ~prog:"/usr/bin/ocamlc"
                        ~args:["-color"; "never"; "-i"; "-impl"; path] ()
  in
  upon out (fun _ -> Unix.remove path >>> ignore);
  out >>| function
  | Error _ -> ""
  | Ok "" -> ""
  | Ok out -> "```ocaml\n" ^ sanitize out ^ "```"

let blank_emoji : Emoji.t = {
  id = None;
  name = "";
  roles = [];
  user = None;
  require_colons = false;
  managed = false;
  animated = false;
}

let timeout_kill : Process.t -> unit =
  let timeout_secs =
    Sys.getenv "DISCORD_OCAML_BOT_TIMEOUT"
    |> Option.bind ~f:float_of_string_opt
    |> Option.value ~default:15.
    |> Time.Span.of_sec
  in fun ps ->
    after timeout_secs >>> fun () ->
    Process.send_signal ps Signal.kill

let run (message : Message.t) (code : string) : Message.t Deferred.Or_error.t =
  let f ps =
    let stdin = Process.stdin ps in
    Writer.write_line stdin wipe_sys;
    Writer.write stdin code;
    timeout_kill ps;
    let%bind output = Process.collect_output_and_wait ps in
    (* TODO: Differ replies based on exit status *)
    begin match output.exit_status with
      | Error (`Exit_non_zero exit_code) -> ignore exit_code
      | Error (`Signal signal) when Signal.(signal = kill) -> ignore signal
      | Error (`Signal signal) -> ignore signal
      | Ok () -> ()
    end;
    let exit_msg = Unix.Exit_or_signal.to_string_hum output.exit_status in
    let%bind out_msg = match output.stdout, output.stderr with 
      | "", "" -> interface code
      | out, "" | "", out -> return @@ "```\n" ^ sanitize out ^ "\n```"
      | stdout, stderr -> return @@
        sprintf "stdout:```\n%s\n```stderr:```\n%s\n```"
                (sanitize stdout) (sanitize stderr)
    in
    let reply_msg = exit_msg ^ "\n" ^ out_msg in
    if String.length reply_msg <= 2000 then
      Message.reply_with ~reply_mention:true ~content:reply_msg message
    else
      Message.reply_with ~reply_mention:true ~content:exit_msg
                         ~files:[("output.txt", reply_msg)] message
  in
  Deferred.Or_error.bind (run_ocaml ()) ~f:f

let check_command (message : Message.t) : unit =
  if Pcre.pmatch ~rex:prefix_pattern message.content then
    try
      let substrings = Pcre.exec ~rex:command_pattern message.content in
      let code = Pcre.get_substring substrings 2 in
      print_endline "Received valid command";
      let emoji = if Random.bool () then "🐪" else "🐫" in
      Message.add_reaction message {blank_emoji with name=emoji} >>> ignore;
      run message code >>> function
      | Ok _ -> print_endline "Successfully replied to command"
      | Error e -> Error.to_string_hum e |> print_endline
    with Caml.Not_found ->
      Message.reply message "Error: Invalid command format" >>> ignore

let main () : unit =
  Client.message_create := check_command;
  Client.start (Sys.getenv_exn "DISCORD_OCAML_BOT_TOKEN") >>> ignore;
  print_endline "Client launched"

let _ =
  Scheduler.go_main ~main ()
