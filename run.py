#!/usr/bin/env python3

from os import listdir
from os.path import isfile, isdir, join
import subprocess

C_END = "\33[0m"
C_BOLD = "\33[1m"
C_ITALIC = "\33[3m"
C_URL = "\33[4m"
C_BLINK = "\33[5m"
C_BLINK_2 = "\33[6m"
C_SELECTED = "\33[7m"

C_BLACK = "\33[30m"
C_RED = "\33[31m"
C_GREEN = "\33[32m"
C_YELLOW = "\33[33m"
C_BLUE = "\33[34m"
C_VIOLET = "\33[35m"
C_BEIGE = "\33[36m"
C_WHITE = "\33[37m"

C_BLACK_BG = "\33[40m"
C_RED_BG = "\33[41m"
C_GREEN_BG = "\33[42m"
C_YELLOW_BG = "\33[43m"
C_BLUE_BG = "\33[44m"
C_VIOLET_BG = "\33[45m"
C_BEIGE_BG = "\33[46m"
C_WHITE_BG = "\33[47m"

C_BLACK_2 = "\33[90m"
C_RED_2 = "\33[91m"
C_GREEN_2 = "\33[92m"
C_YELLOW_2 = "\33[93m"
C_BLUE_2 = "\33[94m"
C_VIOLET_2 = "\33[95m"
C_BEIGE_2 = "\33[96m"
C_WHITE_2 = "\33[97m"

DIRECTORY = "examples"


PRINT = print


def print(text=""):
    PRINT(f"[run.py] {text}")


def run_program(file) -> subprocess.CompletedProcess[bytes]:
    process = subprocess.run(
        ["cargo", "run", "--", file], stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )

    if process.returncode != 0:
        print(f"{C_RED}Error running `{file}`{C_END}")
        print(f"{C_RED}Command: {' '.join(process.args)}{C_END}")
        print(f"{C_RED}Return code: {process.returncode}{C_END}")
        print(f"{C_RED}Stderr:{C_END}")
        stderr = process.stderr.decode().removesuffix("\n")
        PRINT(f"{C_BLACK_2}{stderr}{C_END}")
    else:
        print(f"{C_GREEN}`{file}` successfully compiled{C_END}")

    return process


def main():
    dirs_in_directory = sorted(
        [f for f in listdir(DIRECTORY) if isdir(join(DIRECTORY, f))]
    )

    print(f"Running {len(dirs_in_directory)} programs in `{DIRECTORY}` directory...")

    exit_code = 0
    failed_programs = []

    for program_dir in dirs_in_directory:
        print()
        print(f"Running `{program_dir}`...")

        program_file = join(DIRECTORY, program_dir, "program.bl")

        if isfile(program_file):
            process = run_program(program_file)
            output = process.stdout.removesuffix(b"\n")

            check_file = join(DIRECTORY, program_dir, "check")

            if isfile(check_file):
                with open(check_file, "r") as f:
                    expected_output = f.read().removesuffix("\n")

                if output.decode() == expected_output:
                    print(
                        f"{C_GREEN}`{program_dir}` matched the expected output.{C_END}"
                    )
                else:
                    failed_programs.append(program_dir)

                    print(
                        f"{C_RED}`{program_dir}` didnt match the expected output.{C_END}"
                    )

                    if len(output) > 0 or True:
                        print(f"{C_RED}Expected output:{C_END}")
                        PRINT(f"{C_BEIGE}{expected_output}{C_END}")

                        print(f"{C_RED}Actual output:{C_END}")
                        PRINT(f"{C_VIOLET}{output.decode()}{C_END}")

                    exit_code = 1
            else:
                print(f"{C_RED}`{program_dir}` does not contain a check file.{C_END}")
        else:
            print(
                f"{C_YELLOW}Skipping `{program_dir}` because it does not contain a program file.{C_END}"
            )

    print()
    print(f"Finished running {len(dirs_in_directory)} programs.")
    print(
        f"{C_GREEN}All programs passed.{C_END}"
        if exit_code == 0
        else f"{C_RED}{len(failed_programs)} of {len(dirs_in_directory)} programs failed.{C_END}"
    )
    if len(failed_programs) != 0:
        print(f"{C_RED}Failed programs:{C_END}")
        for program in failed_programs:
            print(f"{C_RED} - {program}{C_END}")
    print(
        f"{C_GREEN if exit_code == 0 else C_RED}Exiting with code {exit_code}.{C_END}"
    )

    exit(exit_code)


if __name__ == "__main__":
    main()
