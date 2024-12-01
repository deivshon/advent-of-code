import os
import requests

from shared import log
from getpass import getpass
from datetime import datetime, timezone

SELF_PATH = os.path.dirname(os.path.realpath(__file__))


def download_puzzle_input(year: int, day: int, session_cookie: str, output_path: str) -> None:
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    response = requests.get(
        url, cookies={"session": session_cookie})
    if response.status_code != 200:
        log.failure(f"could not get input: server responded with {
                    response.status_code}: {response.reason}")

    puzzle_input = response.content.decode()
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, "w") as f:
        f.write(f"{puzzle_input}\n")


current_date = datetime.now(timezone.utc)
last_year = current_date.year if current_date.month == 12 and current_date.hour >= 5 else current_date.year - 1
last_day = 25 if current_date.day >= 25 or current_date.year != last_year else current_date.day

session_cookie = None
for year in range(2015, last_year + 1):
    for day in range(1, last_day + 1 if year == last_year else 25 + 1):
        puzzle_input_output_path = os.path.join(
            SELF_PATH, "..", "puzzle-inputs", str(year), f"{day}.txt")
        if os.path.exists(puzzle_input_output_path):
            continue
        if session_cookie is None:
            session_cookie = getpass("Enter your session cookie: ")

        download_puzzle_input(year, day, session_cookie,
                              puzzle_input_output_path)
        print(f"Downloaded puzzle input for day {day} of year {year}")
