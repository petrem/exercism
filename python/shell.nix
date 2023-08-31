let findNixDir = dir:
      let nixDir = "${dir}/nix" ;
      in if builtins.pathExists nixDir
         then nixDir
         else
           if dir == "/"
           then abort "Cannot find a nix directory uphill"
           else findNixDir (dirOf dir);
    dirName = baseNameOf ./.;
    track = if dirName != "exercism"
            then dirName
            else null;
in (import (findNixDir ./. + "/default.nix") {inherit track;}).shell
