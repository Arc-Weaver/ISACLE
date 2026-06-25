library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tiny_sys_tb is
end entity tiny_sys_tb;

architecture sim of tiny_sys_tb is
    signal clk       : std_logic := '0';
    signal rst       : std_logic := '1';
    signal gpio_port : unsigned(7 downto 0);
    signal gpio_ddr  : unsigned(7 downto 0);
begin
    dut : entity work.tiny_sys port map (
        sys                      => clk,
        rst                      => rst,
        gpio0_GpioPhys_gpioPort  => gpio_port,
        gpio0_GpioPhys_gpioDdr   => gpio_ddr
    );

    clk <= not clk after 10 ns;  -- 50 MHz
    rst <= '0' after 25 ns;      -- release reset after 1.25 cycles

    process
    begin
        wait for 400 ns;  -- extra time for reset + 15 clock cycles
        assert gpio_port = to_unsigned(5, 8)
            report "FAIL: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 5"
            severity error;
        report "gpio_port = " & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = " & integer'image(to_integer(gpio_ddr));
        wait for 700 ns;  -- run another 35 cycles so the stable region is visible
        std.env.stop;
    end process;
end architecture sim;
