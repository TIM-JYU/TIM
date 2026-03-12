def convert_pydoctest_verbose(out):
    # Convert pytest output to more compact form
    # Filter verbose doctest output to compact form (like non-verbose)
    lines = out.splitlines(keepends=True)
    out = ""
    print_block = False

    for line in lines:
        # Start of a failure block
        if line.startswith(
            "****************************************************************"
        ):
            print_block = True
            out += line
            continue

        # Filter out unnecessary verbose lines outside failure block
        if (
            line.startswith("Trying:")
            or line.startswith("Expecting:")
            or line.startswith("ok")
        ):
            print_block = False
            continue
        if "items had no tests" in line:
            print_block = False
            continue
        if "item had no tests" in line:
            print_block = False
            continue
        if "tests in" in line:
            continue
        if "passed and" in line:
            continue

        # Only change "items had failures" to singular form
        if "items had failures" in line:
            line = line.replace("items had failures", "item had failures")
            out += line
            continue

        # Lines inside a failure block
        if print_block:
            out += line
            # Empty line ends failure block
            if line.strip() == "":
                print_block = False
            continue

    return out
