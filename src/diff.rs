/**
 * A modified version of Paul Heckel's diffing algorithm that only works with unique keys.
 *
 * See https://dl.acm.org/citation.cfm?doid=359460.359467.
 */
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Copy, Clone, Debug)]
pub enum Action {
    Insert(usize),
    Remove(usize),
    /** from, to, old_index */
    Move(usize, usize, usize),
    /** index, old_index */
    Update(usize, usize),
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Counter {
    Zero,
    One,
    Many,
}

fn increment(counter: Counter) -> Counter {
    match counter {
        Counter::Zero => Counter::One,
        Counter::One => Counter::Many,
        Counter::Many => Counter::Many,
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct SymbolEntry {
    /// The line's number of occurrences in O.
    oc: Counter,
    /// The line's number of occurrences in N.
    nc: Counter,
    olno: usize,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Entry {
    Symbol,
    Index(usize),
}

pub fn diff<T: Eq + Hash + Copy>(old: &[T], new: &[T]) -> Vec<Action> {
    let mut table: HashMap<T, SymbolEntry> = HashMap::new();
    let (mut oa, mut na) = (Vec::with_capacity(old.len()), Vec::with_capacity(new.len()));

    // Pass 1.
    for item in new.iter() {
        table
            .entry(*item)
            .and_modify(|e| e.nc = increment(e.nc))
            .or_insert(SymbolEntry {
                oc: Counter::Zero,
                nc: Counter::One,
                olno: 0,
            });
        na.push(Entry::Symbol);
    }

    // Pass 2.
    for (index, item) in old.iter().enumerate() {
        table
            .entry(*item)
            .and_modify(|e| {
                e.oc = increment(e.oc);
                e.olno = index;
            })
            .or_insert(SymbolEntry {
                oc: Counter::One,
                nc: Counter::Zero,
                olno: index,
            });
        oa.push(Entry::Symbol);
    }

    // Step 3-5.
    for i in 0..new.len() {
        let entry = table[&new[i]];
        let is_observation1 = entry.nc == Counter::One && entry.oc == Counter::One;
        let is_observation2 =
            entry.nc != Counter::Zero && entry.oc != Counter::Zero && na[i] == oa[entry.olno];
        if is_observation1 || is_observation2 {
            na[i] = Entry::Index(entry.olno);
            oa[entry.olno] = Entry::Index(i);
        }
    }

    let mut actions = Vec::new();

    // `old_index - delete_offsets[old_index]` is the actual index after removals
    let mut offsets = vec![0; old.len()];
    let mut delete_offset = 0;
    for (index, item) in oa.iter().enumerate() {
        offsets[index] = -(delete_offset as isize);
        if let Entry::Symbol = item {
            actions.push(Action::Remove(index - delete_offset));
            delete_offset += 1;
        }
    }

    let mut eoffset = 0;
    for (index, item) in na.iter().enumerate() {
        let eidx = eoffset as isize + *offsets.get(eoffset).unwrap_or(&0);
        match *item {
            Entry::Symbol => {
                actions.push(Action::Insert(index));
                for i in 0..old.len() {
                    if i as isize + offsets[i] >= eidx {
                        offsets[i] += 1;
                    }
                }
            }
            Entry::Index(old_index) => {
                let from = old_index as isize + offsets[old_index];
                if from as usize != index {
                    // The object is not at the expected position: move it
                    actions.push(Action::Move(from as usize, index, old_index));

                    let (lo, hi, d) = if from < eidx {
                        (from, eidx, -1)
                    } else {
                        (eidx, from, 1)
                    };
                    for i in 0..old.len() {
                        let idx = i as isize + offsets[i];
                        if idx >= lo && idx < hi {
                            offsets[i] += d;
                        }
                    }
                } else {
                    eoffset += 1;
                    // The object might have changed
                    actions.push(Action::Update(index, old_index));
                }
            }
        }
    }

    actions
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
