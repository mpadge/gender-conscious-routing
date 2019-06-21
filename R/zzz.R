# nocov start
.onLoad <- function (libname, pkgname)
{
    # This sets the location of the bundled dictionary as an envvar, so it can
    # be read directly in the C code as the FIRST_NAME_FILE macro in gen_ext.h
    loc <- system.file (package = "genderconsciousrouting",
                        "dict", "nam_dict.txt")
    Sys.setenv ("GCR_DICT_DIR" = loc)
    invisible ()
}
# nocov end

