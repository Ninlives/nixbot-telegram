with builtins; 
import (fetchTarball {
  name   = "pin-nixpkgs";
  url    = "https://github.com/nixos/nixpkgs/archive/1526f4534d360eb654e012fe2225235ad42b804e.tar.gz";
  sha256 = "1fh2zyx1yd8hv52pv9610rh2l2xy6sm5q6pm4n53fr0g0yap47nj";
}) { config = {}; overlays = []; }
