# Matrix Multiplication FPGA Accelerator
VHDL components for a Matrix Multiplication/Neural Network Accelerator.

<p align="center">
 <image src="https://github.com/aiaskarioris/mm_fpga_accelerator/images/banner.png" alt="banner"></image>
</p>

## Introduction
This repository contains VHDL source code for a number of digital design components constituting a Matrix Multiplication - AI Accelerator, intended for implementation on an FPGA. The components range from primitive building blocks such as dual-port memories and register pipelines to more advanced and specialized purposes. Together, the components can be put together to form an AI accelerator, called `mat_mult_accelerator`. This accelerator uses the AXI-Stream and AXI-Lite protocols for data input/output and configuration. As such, in practice a complete design would connect a `mat_mult_accelerator` with the PS using DMA blocks and optionally FIFOs in between (at least in a Zynq Ultrascale FPGA).

The accelerator is primarily used in two different methods: the *Matrix Multiplication Mode (MM)* and the *AI Mode*. Despite the name, MM Mode implements a vector-by-matrix multiplication. To use the accelerator the PS must first supply the input vector via the AXI-Stream input interface and then transmit the input matrix via the same interface. While the matrix is being transferred the accelerator performs the necessary operations as such that by the end of the matrix's transmission, only a short latency period occurs before the results are sent back to PS.

<p align="center">
 <image src="https://github.com/aiaskarioris/mm_fpga_accelerator/images/core_logic_arch.png" alt="components"></image>
</p>

AI Mode performs two vector-by-matrix multiplications and sums the results in addition to a bias vector. This operation is encountered in LSTM networks, within LSTM cells. Afterwards, the final result can pass through an activation function (e.g. sigmoid), which is implemented using a look-up table. The look-up table must be supplied by PS and the accelerator is set up to have two store tables, thus allowing fast switching between the two.

## Motivation
This code base was developed for my thesis in the Department of Informatics and Computer Engineering, University of West Attica, titled *Study and Customization of Artificial Intelligence on SoC-FPGA for
Audio Signal Processing* ([link to PDF](https://polynoe.lib.uniwa.gr/xmlui/bitstream/handle/11400/9983/Karioris_19390079.pdf?sequence=1&isAllowed=y)). Note  the first few pages are not written in english but the thesis itself is. The goal of this work was to accelerate a Neural Network DSP algorithm for audio utilizing an SoC+FPGA system. In the final design, the accelerator would perform all vector-by-matrix multiplications in addition to a few extra operations regarding the vector products, thus significantly accelerating the execution of LSTM networks. Since the implemented algorithm did not require any matrix-by-matrix multiplications, only vector-by-matrix multiplications has been implemented. A detailed overview of the individual components as well as their integration can be found in [Chapters 5.2](https://polynoe.lib.uniwa.gr/xmlui/bitstream/handle/11400/9983/Karioris_19390079.pdf?sequence=1&isAllowed=y#section.5.2) & [5.3](https://polynoe.lib.uniwa.gr/xmlui/bitstream/handle/11400/9983/Karioris_19390079.pdf?sequence=1&isAllowed=y#section.5.3) of the thesis. Utilizing the FPGA accelerator alongside SIMD operations on the PS side, the algorithm was sped up by more than x10.0 and by x1.8 compared to only the use of PS (with SIMD). More evaluation details can be found in [Chapters 6.4](https://polynoe.lib.uniwa.gr/xmlui/bitstream/handle/11400/9983/Karioris_19390079.pdf?sequence=1&isAllowed=y#section.6.4) and [6.5](https://polynoe.lib.uniwa.gr/xmlui/bitstream/handle/11400/9983/Karioris_19390079.pdf?sequence=1&isAllowed=y#section.6.5).

## Repository Structure
In the `components` directory the VHDL code of the components can be found. In the `testbenches` directory VHDL testbenches for the validation of the components can be found.

## Development
This code was developed with *Vivado 2023.2*. The target platform was the *Xilinx ZCU104 Evaluation Board* but due to time limitations the code itself was not tested on the actual hardware. For the same reason the VHDL components were not tested in an actual PS+PL design and instead all evaluation numbers are theoretical estimates (in part based on Vivado's reports).

### Importing to Vivado
To import the project to Vivado simply include `components` as a Source Design. The source files must be set to type **VHDL 2008** in order for linting errors to stop being generated. Vivado should be able to select component `mat_mult_accelerator` as the top component automatically. 
