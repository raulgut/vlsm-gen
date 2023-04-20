# vlsm-gen
A simple Variable Length  Subnet Mask (VLSM) generator

It accepts a file as an input (see examples) with the following information:

1) The Intial Address (the one given by your ISP)
2) The associated mask
3) How you want to assign the subnet addressed, two options:
  a) MINIMUM: allocates on the first empty spot in the address space.
  b) SEQUENTIAL: allocates each subnet after the last broadcast address.
4) Subnet sizes (one each line)
