#run {
    set_build_options_dc(.{do_output = false});

    w := compiler_create_workspace();
    options := get_build_options();
    options.output_type = .EXECUTABLE;
    options.output_executable_name = "main";

    import_path: [..] string;
    array_copy(*import_path, options.import_path);
    array_add(*import_path, "../../");
    options.import_path = import_path;

    set_build_options(options, w);

    add_build_file("example.jai", w);
}

#import "Compiler";
#import "Basic";

