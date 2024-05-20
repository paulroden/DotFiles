# stand-in to use updated DockUtil version
# until merge of PR #293032 <https://github.com/NixOS/nixpkgs/pull/293032>
(final: prev: {
  dockutil = prev.dockutil.overrideAttrs (_: {
    src =
      let version = "3.1.3";
          downloadBaseURL = "https://github.com/kcrawford/dockutil/releases/download";
      in prev.fetchurl {
        url = "${downloadBaseURL}/${version}/dockutil-${version}.pkg";
        sha256 =
          "f60db8273fe80d7c4824588bedb538a5387675c3917edb0010dab9f52d3f2582";
      };
  });
})
