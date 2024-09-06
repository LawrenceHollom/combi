use std::fs::{File, OpenOptions};
use fs2::FileExt;
use std::io::Write;

/**
 * Write lines to a file; also keeps track of lock.
 */
pub fn write(directory: &str, filename: &str, lines: Vec<String>, append: bool) {
    let mut pathbuf = std::env::current_exe().unwrap();
    pathbuf.pop();
    pathbuf.push(directory);
    let mut live_file = pathbuf.to_owned();
    live_file.push(format!("{}.txt", filename));
    let mut lock_file = pathbuf.to_owned();
    lock_file.push("lock");

    if !lock_file.exists() {
        println!("FAILURE TO SAVE: LOCK FILE NOT FOUND!");
        return;
    }

    // Wait to get the lock if necessary.
    let lock = File::open(lock_file).unwrap();
    lock.lock_exclusive().unwrap();

    let mut live_file = OpenOptions::new().append(append).create(true).open(live_file).unwrap();
    for line in lines.iter() {
        writeln!(&mut live_file, "{}", line).unwrap();
    }
    
    lock.unlock().unwrap();
}