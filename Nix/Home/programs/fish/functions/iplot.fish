# modified from: https://sw.kovidgoyal.net/kitty/integrations/#tool-gnuplot
set plot_cmd "\
set terminal pngcairo enhanced font 'Fira Sans,10'
set autoscale
set samples 1000
set output '|kitten icat --stdin yes'
set object 1 rectangle from screen 0,0 to screen 1,1 fillcolor rgb\"#fdf6e3\" behind
plot $argv
set output '/dev/null'"

echo $plot_cmd | gnuplot
