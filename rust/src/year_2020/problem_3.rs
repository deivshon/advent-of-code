enum Tile {
    Free,
    Tree,
}

struct Point {
    x: usize,
    y: usize,
}

struct Slope {
    x_increment: usize,
    y_increment: usize,
}

struct Area {
    row_length: usize,
    map: Vec<Vec<Tile>>,
}

impl Area {
    fn count_trees(&self, start: Point, slope: Slope) -> i64 {
        let mut x = start.x;
        let mut y = start.y;

        let mut trees: i64 = 0;
        while y < self.map.len() {
            let current = &self.map[y][x % self.row_length];
            if matches!(current, Tile::Tree) {
                trees += 1;
            }

            x += slope.x_increment;
            y += slope.y_increment;
        }

        return trees;
    }
}

enum AreaParseError {
    MalformedArea,
    UnknownTile,
    NoRowLength,
}

impl TryFrom<String> for Area {
    type Error = AreaParseError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let mut map: Vec<Vec<Tile>> = Vec::new();

        let mut row_length: Option<usize> = None;
        for row in value.lines().filter(|x| !x.is_empty()) {
            match row_length {
                Some(expected) => {
                    if expected != row.len() {
                        return Err(AreaParseError::MalformedArea);
                    }
                }
                None => row_length = Some(row.len()),
            }

            let mut tiles_row: Vec<Tile> = Vec::new();
            for tile in row.chars() {
                match tile {
                    '.' => tiles_row.push(Tile::Free),
                    '#' => tiles_row.push(Tile::Tree),
                    _ => return Err(AreaParseError::UnknownTile),
                }
            }

            map.push(tiles_row);
        }

        return match row_length {
            Some(row_length) => Ok(Area { map, row_length }),
            None => Err(AreaParseError::NoRowLength),
        };
    }
}

pub fn part_1(input: String) -> Option<String> {
    let area = Area::try_from(input).ok()?;

    return Some(
        area.count_trees(
            Point { x: 0, y: 0 },
            Slope {
                x_increment: 3,
                y_increment: 1,
            },
        )
        .to_string(),
    );
}

pub fn part_2(input: String) -> Option<String> {
    let area = Area::try_from(input).ok()?;
    let slopes = [
        Slope {
            x_increment: 1,
            y_increment: 1,
        },
        Slope {
            x_increment: 3,
            y_increment: 1,
        },
        Slope {
            x_increment: 5,
            y_increment: 1,
        },
        Slope {
            x_increment: 7,
            y_increment: 1,
        },
        Slope {
            x_increment: 1,
            y_increment: 2,
        },
    ];

    return slopes
        .map(|slope| area.count_trees(Point { x: 0, y: 0 }, slope))
        .into_iter()
        .reduce(|acc, n| acc * n)
        .map(|sum| sum.to_string());
}
