use std::ops::{Add, Div, Sub};

use image::{DynamicImage, ImageBuffer, ImageReader, Rgb};
use rand::{rngs::ThreadRng, thread_rng, Rng};

use crate::entity::graph::*;
use utilities::vertex_tools::*;

#[derive(Debug, Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

struct Embedding {
    positions: VertexVec<Point>,
}

struct Printer {
    node_bg: DynamicImage,
    numbers: DynamicImage,
    width: u32,
    height: u32,
}

impl Point {
    pub const ZERO: Point = Point { x: 0.0, y: 0.0 };

    pub fn new_random(rng: &mut ThreadRng) -> Point {
        Point {
            x: rng.gen_range(0.0..1.0),
            y: rng.gen_range(0.0..1.0),
        }
    }

    pub fn to_pixel_coords(&self, width: u32, height: u32) -> Point {
        let x = (width as f64) * (self.x + 0.1) / 1.2;
        let y = (height as f64) * (self.y + 0.1) / 1.2;
        Point { x, y }
    }

    pub fn length(&self) -> f64 {
        ((self.x * self.x) + (self.y * self.y)).sqrt()
    }
}

impl Embedding {
    pub fn new(g: &Graph, rng: &mut ThreadRng) -> Embedding {
        let mut positions = VertexVec::new(g.n, &Point::ZERO);
        for pos in positions.iter_mut() {
            *pos = Point::new_random(rng);
        }
        Embedding { positions }
    }

    /** Shuffle stuff around to try and find the perfect embedding. */
    pub fn optimise(&mut self) {

    }

    pub fn get(&self, v: Vertex) -> Point {
        self.positions[v]
    }
}

impl Printer {
    const WHITE: Rgb<u8> = Rgb([255, 255, 255]);
    const BLACK: Rgb<u8> = Rgb([0, 0, 0]);

    pub fn new() -> Printer {
        Printer {
            node_bg: read_image("base"),
            numbers: read_image("numbers"),
            width: 1000,
            height: 1000,
        }
    }

    fn draw_line(&self, imgbuf: &mut ImageBuffer<Rgb<u8>, Vec<u8>>, start_point: Point, end_point: Point) {
        let start = start_point.to_pixel_coords(self.width, self.height);
        let end = end_point.to_pixel_coords(self.width, self.height);
        let length = (end - start).length();
        let step = (end - start) / (length * 5.0);
        let num_steps = (length * 5.0) as usize;
        let mut pos = start;
        for _i in 0..num_steps {
            pos = pos + step;
            let x = pos.x.round() as u32;
            let y = pos.y.round() as u32;
            *imgbuf.get_pixel_mut(x, y) = Self::BLACK;
        }
    }

    fn draw_node(&self, imgbuf: &mut ImageBuffer<Rgb<u8>, Vec<u8>>, name: String, start: Point) {

    }

    pub fn print_graph(&self, g: &Graph, embedding: &Embedding) {
        let mut imgbuf: ImageBuffer<Rgb<u8>, Vec<u8>> = image::ImageBuffer::new(self.width, self.height);

        for px in imgbuf.pixels_mut() {
            *px = Self::WHITE;
        }

        // Draw all the edges.
        for e in g.iter_edges() {
            self.draw_line(&mut imgbuf, embedding.get(e.fst()), embedding.get(e.snd()))
        }

        // Draw all the node blobs and numbers
        for v in g.iter_verts() {
            self.draw_node(&mut imgbuf, v.to_string(), embedding.get(v))
        }

        save_buffer(&imgbuf, "output");
    }
}

pub fn print_graph(g: &Graph) {
    let printer = Printer::new();
    let mut rng = thread_rng();

    // Work out where the points should all go.
    let mut embedding = Embedding::new(g, &mut rng);
    embedding.optimise();

    // Actually print the graph.
    printer.print_graph(g, &embedding);
}

pub fn read_image(filename: &str) -> DynamicImage {
    let filename = format!("images/{}.png", filename);
    let img = ImageReader::open(filename).unwrap().decode().unwrap();
    img
}

pub fn save_buffer(imgbuf: &ImageBuffer<image::Rgb<u8>, Vec<u8>>, filename: &str) {
    let filename = format!("images/{}.png", filename);
    imgbuf.save(filename).unwrap()
}

impl Sub<Point> for Point {
    type Output = Point;

    fn sub(self, rhs: Point) -> Self::Output {
        Point {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl Add<Point> for Point {
    type Output = Point;

    fn add(self, rhs: Point) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Div<f64> for Point {
    type Output = Point;

    fn div(self, rhs: f64) -> Self::Output {
        Point {
            x: self.x / rhs,
            y: self.y / rhs,
        }
    }
}