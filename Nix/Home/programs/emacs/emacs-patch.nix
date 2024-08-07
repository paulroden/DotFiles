final: prev: pkgs: {
  inherit (pkgs);
  emacs' = prev.emacs.overrideAttrs (prev: {
    # Use the beautiful squircle icon from Noboyuki Sato
    postUnpack = (prev.postUnpack or "") + ''
      cp ${./icons/nobu417-big-sur.icns} $sourceRoot/nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
    '';
    patches =
      let patchesBaseURL =
            "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches";
      in (prev.patches or []) ++ [
        # Fix OS window role (needed for window managers like yabai)
        (pkgs.fetchpatch {
          url = "${patchesBaseURL}/emacs-28/fix-window-role.patch";
          sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
        })
        # Use poll instead of select to get file descriptors
        (pkgs.fetchpatch {
          url = "${patchesBaseURL}/emacs-30/poll.patch";
          sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
        })
        # leave frame selecting to Emacs
        (pkgs.fetchpatch {
          url = "${patchesBaseURL}/emacs-28/no-frame-refocus-cocoa.patch";
          sha256 = "sha256-QLGplGoRpM4qgrIAJIbVJJsa4xj34axwT3LiWt++j/c=";
        })
        # Make Emacs aware of OS-level light/dark mode
        (pkgs.fetchpatch {
          url = "${patchesBaseURL}/emacs-28/system-appearance.patch";
          sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
        })
        # undecorated frames with round corners
        (pkgs.fetchpatch {
          url = "${patchesBaseURL}/emacs-30/round-undecorated-frame.patch";
          sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
        })
      ];
  });
}
