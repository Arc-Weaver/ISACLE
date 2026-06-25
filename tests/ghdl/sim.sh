#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/../.."

# 1. Synthesise VHDL
cabal run test-tiny-sys

VHDL=build/tiny_sys
TB=tests/ghdl

# 2. Analyse all design files + testbench
ghdl -a --std=08 \
    "$VHDL/cpu0.vhd" \
    "$VHDL/databus.vhd" \
    "$VHDL/gpio0.vhd" \
    "$VHDL/tiny_sys.vhd" \
    "$TB/tiny_sys_tb.vhd"

# 3. Elaborate
ghdl -e --std=08 tiny_sys_tb

# 4. Simulate, dump VCD for waveform inspection
ghdl -r --std=08 tiny_sys_tb \
    --vcd="$VHDL/tiny_sys.vcd" \
    --stop-time=2000ns

echo "VCD written to $VHDL/tiny_sys.vcd"
echo "View with: gtkwave $VHDL/tiny_sys.vcd"
