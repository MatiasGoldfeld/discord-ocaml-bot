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

let run_ocaml = Process.create ~prog:"/usr/bin/ocaml"
    ~args:["-color"; "never"; "-stdin"; "-noinit"]

let timeout_kill (ps : Process.t) (seconds : float) : unit =
  after (Time.Span.of_sec seconds) >>> fun () ->
  Process.send_signal ps Signal.kill

let run (message : Message.t) (code : string) : Message.t Deferred.Or_error.t =
  run_ocaml () >>= function
  | Error _ as e ->
    print_endline "Error creating new process";
    return e
  | Ok ps ->
    let stdin = Process.stdin ps in
    Writer.write_line stdin wipe_sys;
    Writer.write stdin code;
      Process.pid ps
  |> Pid.to_string
  |> print_endline;
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
    if String.length reply_msg <= 2000 then
      Message.reply_with ~content:reply_msg message
    else
      Message.reply_with ~content:exit_msg ~files:[("output.txt", reply_msg)] message

let check_command (message : Message.t) : unit =
  if Pcre.pmatch ~rex:prefix_pattern message.content then
    try
      let substrings = Pcre.exec ~rex:command_pattern message.content in
      let code = Pcre.get_substring substrings 2 in
      print_endline "Received valid command";
      run message code >>> function
      | Ok _ -> print_endline "Success"
      | Error e -> Error.to_string_hum e |> print_endline
    with Caml.Not_found ->
      Message.reply message "Error: Invalid command format." >>> ignore

let main () : unit =
  Client.message_create := check_command;
  Client.start (Sys.getenv_exn "DISCORD_OCAML_BOT_TOKEN") >>> ignore;
  print_endline "Client launched"

let _ =
  Scheduler.go_main ~main ()
