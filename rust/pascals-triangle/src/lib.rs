pub struct PascalsTriangle {

    _rows: Vec<Vec<u32>>,

}

impl PascalsTriangle {

    pub fn new(row_count: u32) -> Self {

        let mut rows = Vec::new();

        for i in 0..(row_count as usize) {

            if i == 0 {

                rows.push(vec![1]);

            } else {

                let mut cur_row = Vec::new();

                {
                    let ref last_row = rows[i - 1];

                    let points;
                    {
                        let lefts = -1..last_row.len() as i32;
                        let rights = 0..last_row.len() as i32 + 1;
                        points = lefts.zip(rights);
                    }

                    for (il, ir) in points {
                        let left = if il < 0 { 0 } else { last_row[il as usize] };
                        let right = *last_row.get(ir as usize).unwrap_or(&0);
                        cur_row.push(left + right)
                    }
                }

                rows.push(cur_row);

            }

        }

        PascalsTriangle { _rows: rows }
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {

        self._rows.clone()

    }

}
