import Lake
open Lake DSL

package tv where
  moreLinkArgs := #["-L/usr/local/lib", "-ltermbox2", "-Wl,--allow-shlib-undefined"]

lean_lib Tv where
  roots := #[`Tv.Types, `Tv.Viewport, `Tv.Term, `Tv.Csv, `Tv.Render, `Tv.App]

@[default_target]
lean_exe tv where
  root := `Main
  moreLinkArgs := #["c/libtermshim.a", "-L/usr/local/lib", "-ltermbox2", "-Wl,--allow-shlib-undefined"]
