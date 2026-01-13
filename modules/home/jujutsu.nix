{ ... }:
{
  programs.jjui.enable = true;
  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = "thelissimus";
        email = "thelissimus@tuta.io";
      };
    };
  };
}
