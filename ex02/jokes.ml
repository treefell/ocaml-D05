let randomPick ()=
    Random.self_init ();
    (Random.int 5)

let () =
    let jokes = [|"How t did the Boston Marathon bombers do that Hitler couldn't?
    Ended a race.";
    "What's the difference between a trunk full of bowling balls and a trunk full of dead babies?
    You can't unload bowling balls with pitchfork.";
    "Why are black men good at basketball?
    The whole purpose is to run shoot and steal.";
    "Say what what you will about pedofiles but hey they do drive slower through school zones.";
    "I was fucking my daughter the other night, and I don't know what was funnier.The look on my wife's face when she walked in on me or the fact that the abortion clinic let me keep her."|]
    in
    print_endline (jokes.(randomPick()))
