open Core
open Async
open Disml
open Models

let prefix_pattern = Pcre.regexp "^!ocaml(?:\\s|```)"
let command_pattern = Pcre.regexp
    "^!ocaml(?:\\s+|(?=```))(`{0,3})(?:(?<=```)ocaml\n)?([\\s\\S]+)\\1"

let wipe_sys = 
  "module Sys = struct end \
   module Stdlib = struct include Stdlib module Sys = struct end end;;"

let run_ocaml = Process.create ~prog:"/usr/bin/env"
    ~args:["ocaml"; "-color"; "never"; "-stdin"; "-noinit"]

let timeout_kill (ps : Process.t) (seconds : float) : unit =
  after (Time.Span.of_sec seconds) >>> fun () ->
  Process.send_signal ps Signal.kill

let temp (message : Message.t) (code : string) : Message.t Deferred.Or_error.t =
  run_ocaml () >>= function
  | Error _ as e ->
    print_endline "Error creating new process";
    return e
  | Ok ps ->
    let stdin = Process.stdin ps in
    Writer.write_line stdin wipe_sys;
    Writer.write stdin code;
    timeout_kill ps 15.;
    let%bind output = Process.collect_output_and_wait ps in
    begin match output.exit_status with
      | Error (`Exit_non_zero exit_code) -> ignore exit_code
      | Error (`Signal signal) when Signal.(signal = kill) -> ignore signal
      | Error (`Signal signal) -> ignore signal
      | Ok () -> ()
    end;
    let exit_msg = Unix.Exit_or_signal.to_string_hum output.exit_status in
    let reply_msg = exit_msg ^ match output.stdout, output.stderr with 
      | "", "" -> ""
      | out, "" | "", out -> "\n```\n" ^ out ^ "\n```"
      | stdout, stderr ->
        "\nstdout:```\n" ^ stdout ^ "\n```stderr:```\n" ^ stderr ^ "\n```"
    in
    print_endline (Int.to_string (String.length reply_msg));
    if String.length reply_msg <= 2000 then
      Message.reply message reply_msg
    else
      let%bind path, fd = Unix.mkstemp "/tmp/discord.ocaml.bot" in
      let writer = Writer.create fd in
      Writer.write writer reply_msg;
      let%bind () = Writer.close writer in
      let%bind ret = Message.reply_with ~file:path ~content:exit_msg message in
      let%map () = Unix.remove path in ret

let check_command (message : Message.t) : unit =
  print_endline message.content;
  if Pcre.pmatch ~rex:prefix_pattern message.content then
    try
      let substrings = Pcre.exec ~rex:command_pattern message.content in
      let code = Pcre.get_substring substrings 2 in
      temp message code >>> ignore
    with Caml.Not_found ->
      Message.reply message "Error: Invalid command format." >>> ignore

let main () : unit =
  Client.message_create := check_command;
  Client.start (Sys.getenv_exn "DISCORD_OCAML_BOT_TOKEN") >>> ignore;
  print_endline "Client launched"

let _ =
  Scheduler.go_main ~main ()
