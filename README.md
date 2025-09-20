# Compression & Decompression (OCaml)

An OCaml implementation of **Huffman coding** for file compression and decompression.  
Developed as part of a coursework project on algorithms and data structures.

## Project Overview
The project demonstrates how Huffman coding can reduce file sizes by encoding frequent symbols with shorter bit patterns and rare symbols with longer ones. It includes both the compression and decompression pipeline.

- **heap.ml / heap.mli** — priority queue for building Huffman trees  
- **bs.ml / bs.mli** — bitstream utilities for reading/writing compressed data  
- **huffman.ml** — Huffman tree construction and encoding  
- **huff.ml** — main program (compression & decompression driver)  
- **rapport.pdf** — project report (design choices and results)  

## Features
- Builds a Huffman tree from character frequencies  
- Generates prefix-free binary codes for each symbol  
- Encodes text into a compressed bitstream  
- Decodes compressed files back to the original text  

## Authors
- Tianwen Gu  
- Hongfei Zhang
