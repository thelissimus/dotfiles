{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    profiles.default = {
      isDefault = true;
      extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
        clearurls
        darkreader
        # dont-accept-image-webp
        # imagus
        i-dont-care-about-cookies
        privacy-badger
        simple-tab-groups
        single-file
        ublock-origin
        unpaywall
        vimium
        youtube-shorts-block
        # user-agent-switcher-manager
      ];
      settings = {
        "browser.newtabpage.activity-stream.feeds.topsites" = false;
        "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
        "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
        "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
        "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.search.region" = "DE";
        "browser.search.suggest.enabled" = false;
        "browser.toolbars.bookmarks.visibility" = "never";
        "browser.urlbar.showSearchSuggestionsFirst" = false;
        "browser.urlbar.suggest.searches" = false;
        "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
        "font.name.monospace.x-western" = "SF Mono";
        "font.name.sans-serif.x-western" = "SF Pro Display";
        "signon.autofillForms" = false;
        "signon.firefoxRelay.feature" = "disabled";
        "signon.generation.enabled" = false;
        "signon.management.page.breach-alerts.enabled" = false;
        "signon.rememberSignons" = false;
      };
    };
  };
}
