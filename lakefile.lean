import Lake
open Lake DSL

package tv where
  moreLinkArgs := #["-L/usr/local/lib", "-ltermbox2", "-Wl,--allow-shlib-undefined"]

lean_lib Tv where
  roots := #[`Tv.Types, `Tv.Term, `Tv.Render, `Tv.State, `Tv.Fzf, `Tv.Prql, `Tv.Source, `Tv.Meta, `Tv.Freq, `Tv.Key, `Tv.App, `Tv.Adbc, `Tv.Backend, `Tv.Error]

@[default_target]
lean_exe tv where
  root := `Main
  moreLeancArgs := #["-g"]
  moreLinkArgs := #["c/libtermshim.a", "c/libadbcshim.a", "-L/usr/local/lib", "-ltermbox2", "-Wl,-rpath,/usr/local/lib", "-Wl,--allow-shlib-undefined"]

lean_exe test where
  root := `Test
  moreLinkArgs := #["c/libtermshim.a", "c/libadbcshim.a", "-L/usr/local/lib", "-ltermbox2", "-Wl,-rpath,/usr/local/lib", "-Wl,--allow-shlib-undefined"]

lean_lib Tc where
  roots := #[`Tc.Offset, `Tc.Nav, `Tc.Render, `Tc.Key, `Tc.App]

lean_lib Qtv where
  roots := #[`Qtv.Types, `Qtv.State, `Qtv.Nav, `Qtv.Op, `Qtv.Render, `Qtv.Key]

lean_exe qtv where
  root := `Qtv.Main
  moreLinkArgs := #["c/libtermshim.a", "-L/usr/local/lib", "-ltermbox2", "-Wl,-rpath,/usr/local/lib"]

lean_exe tc where
  root := `Tc.App
  moreLinkArgs := #["c/libtermshim.a", "c/libadbcshim.a", "-L/usr/local/lib", "-ltermbox2", "-Wl,-rpath,/usr/local/lib", "-Wl,--allow-shlib-undefined"]
