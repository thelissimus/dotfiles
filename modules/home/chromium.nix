{ pkgs, lib, ... }:
{
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions =
      let
        mkChromiumExtensionFor = browserVersion: { id, sha256, version }: {
          inherit id;
          crxPath = pkgs.fetchurl {
            url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
            name = "${id}.crx";
            inherit sha256;
          };
          inherit version;
        };
        mkChromiumExtension = mkChromiumExtensionFor (lib.versions.major pkgs.ungoogled-chromium.version);
      in
      map mkChromiumExtension [
        {
          # clearurls
          id = "lckanjgmijmafbedllaakclkaicjfmnk";
          version = "1.26.0";
          sha256 = "06m3b3npis7cpv0yif0rs3dkfdgd69r0rkyxlwwry26h58dp7hdc";
        }
        {
          # dark-reader
          id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
          version = "4.9.95";
          sha256 = "1lc5ny57rdpqzx07gwj44pl3m9mg9nq6p5kjchrmjjh4hkk5wsny";
        }
        {
          # imagus
          id = "immpkjjlgappgfkkfieppnmlhakdmaab";
          version = "0.9.8.74";
          sha256 = "1xqm3h2l18cmkd74yzxran68vrwrsnf3kkxb78jf8vs6cmdi5vpy";
        }
        {
          # i-still-dont-care-about-cookies
          id = "edibdbjcniadpccecjdfdjjppcpchdlm";
          version = "1.1.4";
          sha256 = "11k7cxcjafs8ziaxl4bilbfwbgl2yf1p6v1bvwszadcr14xyvgsj";
        }
        {
          # privacy-badger
          id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
          version = "2024.7.17";
          sha256 = "0kx6pgk3m9mdqwnh2lxw1cid9csv6xvchdw55svimrl5ky2dqlzx";
        }
        {
          # singlefile
          id = "mpiodijhokgodhhofbcjdecpffjipkle";
          version = "1.22.71";
          sha256 = "0fy8wcw83g3qps9ghqrnxcfyjqmsz4wi3xn2n9ymfvn9yx4l7ffg";
        }
        {
          # ublock-origin
          id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
          version = "1.60.0";
          sha256 = "0pdh1v0vx1d5vnl1zh7nbk6j1fh4k4hhwp1ljs203icn306lahsn";
        }
        {
          # unhook
          id = "khncfooichmfjbepaaaebmommgaepoid";
          version = "1.6.8";
          sha256 = "1wwz47zb11ybgi025pbwi911g3ddzv0pkvgybgddxdnjp874xs4f";
        }
        {
          # vimium
          id = "dbepggeogbaibhgnhhndojpepiihcmeb";
          version = "2.1.2";
          sha256 = "0n1ja4l8pvcsm1xdvq5gqw9f320yaidb6piifbqp3nhgfklif85l";
        }
        {
          # return-youtube-dislike
          id = "gebbhagfogifgggkldgodflihgfeippi";
          version = "3.0.0.17";
          sha256 = "0riya5wwrfx50rwca3c4q83l4blkbzfrnrf8s1g23nyrq8fxkasp";
        }
        {
          # youtube-shorts-block
          id = "jiaopdjbehhjgokpphdfgmapkobbnmjp";
          version = "1.5.3";
          sha256 = "0gs9p5kq62i7yc8k65agpbyay054axswspf0j1bs8hxl0gb43bd7";
        }
        {
          # sponsorblock
          id = "mnjggcdmjocbbbhaepdhchncahnbgone";
          version = "5.9.4";
          sha256 = "0vwzda6dsy9cm8ml9ahxbs3j6zqjjkjfz55zmfixr0xm0aab1c9j";
        }
        {
          # keepassxc
          id = "oboonakemofpalcgghocfoadofidjkkk";
          version = "1.9.3";
          sha256 = "0zlmwiyzn4cznzrvavp2nsj07v4lrdyi7nw5aarqnj7nj12gixh7";
        }
        {
          id = "jnbbnacmeggbgdjgaoojpmhdlkkpblgi";
          version = "4.0.7";
          sha256 = "1x57znbrh70d9dab3p476y11v0dzsp184bp247qijdsg0hkfv92h";
        }
      ];
  };
}
