use std::{collections::HashMap, hash::Hash, ops::AddAssign, sync::mpsc::{self, Sender}, thread::{self, JoinHandle}};

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    let mut workers: Vec<Worker<_>> = vec![];
    let (results_tx, results_rx) = mpsc::channel();
    
    for _ in 0..worker_count {
        let (data_tx, rx) = mpsc::channel();
        let tx = results_tx.clone();
        let handle = thread::spawn(move || {
            while let Ok(data) = rx.recv() {
                tx.send(count_letters(data)).unwrap();
            }
        });
        workers.push(Worker::new(handle/*, data_tx*/));
    }

    for input_chunks in input.chunks(worker_count) {
        for (worker, data) in input_chunks.iter().enumerate() {
           // workers[worker].tx.send(data).unwrap();
        }
    }
    for worker in workers {
        worker.handle.join().unwrap();
    }
    let mut result = HashMap::new();
    while let Ok(partial_result) = results_rx.recv() {
        result = combine_from(result, &partial_result);
    }
    result
            
    //input.iter().fold(HashMap::new(), |acc, batch| combine_from(acc, &count_letters(batch)))
        
}

struct Worker<T> {
    handle: JoinHandle<T>,
//    tx: Sender<D>,
}

impl<T> Worker<T> {
    fn new(handle: JoinHandle<T>/*, tx: Sender<D>*/) -> Self {
        Self {handle/*, tx*/}
    }
}

fn count_letters(input: &str) -> HashMap<char, usize> {
    let mut counter = HashMap::new();
    input.to_lowercase().chars().for_each(|c| if c.is_alphabetic() {
        counter.entry(c).and_modify(|counter| *counter += 1).or_insert(1);});
    counter
}
                                          
fn combine_from<K, V>(mut h1: HashMap<K, V>, h2: &HashMap<K, V>) -> HashMap<K, V>
where K: Copy + PartialEq + Eq + Hash,
      V: AddAssign + Clone {
    for (k2, v2) in h2.iter() {
        h1.entry(*k2).and_modify(|v| *v += v2.clone()).or_insert(v2.clone());
    }
    h1
}
