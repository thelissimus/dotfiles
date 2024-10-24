{ pkgs, lib, ... }:
{
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions =
      let
        mkChromiumExtensionFor = browserVersion: { id, sha256, version }: {
          inherit id;
          crxPath = builtins.fetchurl {
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
          sha256 = "0169harv9niwsjhi8pn3p063k1yhnlgjspih6gcfa6wxil50djzp";
        }
        {
          # imagus
          id = "immpkjjlgappgfkkfieppnmlhakdmaab";
          version = "0.9.8.74";
          sha256 = "19mirfy8ggq2zaxp8clv28aq1lmv5xdlvf9j62ig9p75pr4v3qa1";
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
          sha256 = "0jsqa7v2zdjwwp4gfl98yda6vsii374fl1bwqjynnilj7ah8610z";
        }
        {
          # singlefile
          id = "mpiodijhokgodhhofbcjdecpffjipkle";
          version = "1.22.71";
          sha256 = "12c9j6bfbdnqdb2ncnqwh9rywk351japbrxrfaijx8d1kjcs222m";
        }
        {
          # ublock-origin
          id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
          version = "1.60.0";
          sha256 = "1h7hkch680kp40mrc1121l8rl1qxwplqwbl53ysy5kbp9jn77v1r";
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
          sha256 = "0m8xski05w2r8igj675sxrlkzxlrl59j3a7m0r6c8pwcvka0r88d";
        }
        {
          # return-youtube-dislike
          id = "gebbhagfogifgggkldgodflihgfeippi";
          version = "3.0.0.17";
          sha256 = "0zn8m6r0pwyw6v2kj4ar21q4kzim14m6la7zmx5qz4ikmkmjw7sd";
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
          sha256 = "0ppi2yg478g15sc0w6ixxbhlvrg6pki93k5vf33dl4cigmw9xhbg";
        }
        {
          # keepassxc
          id = "oboonakemofpalcgghocfoadofidjkkk";
          version = "1.9.3";
          sha256 = "0ajgl28fh6m5p4jl1v60rglxksbi547xv1awh3kp42x510yk4w58";
        }
        {
          id = "jnbbnacmeggbgdjgaoojpmhdlkkpblgi";
          version = "4.0.7";
          sha256 = "1g10zsm2d30937kl994jbxidnnsrpzhmi6if69mxmwcp4rnm1z09";
        }
      ];
  };
}
