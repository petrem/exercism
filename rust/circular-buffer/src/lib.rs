use std::{collections::VecDeque, fmt::Debug};

#[derive(Debug)]
pub struct CircularBuffer<T> {
    ring: VecDeque<T>,
    capacity: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

impl<T: Debug> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        Self {
            ring: VecDeque::with_capacity(capacity),
            capacity,
        }
    }

    pub fn write(&mut self, element: T) -> Result<(), Error> {
        if self.ring.len() == self.capacity {
            return Err(Error::FullBuffer);
        }
        self.ring.push_back(element);
        Ok(())
    }

    pub fn read(&mut self) -> Result<T, Error> {
        self.ring.pop_front().ok_or(Error::EmptyBuffer)
    }

    pub fn clear(&mut self) {
        self.ring.truncate(0);
    }

    pub fn overwrite(&mut self, element: T) {
        // to avoid requiring T: Clone, reimplement `write` functionality
        // instead of calling it
        if self.ring.len() == self.capacity {
            _ = self.ring.pop_front();
            self.ring.push_back(element)
        } else {
            self.ring.push_back(element);
        }
    }
}
