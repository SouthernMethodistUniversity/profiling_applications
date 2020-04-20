import pstats
p = pstats.Stats('profile_data.txt')
p.strip_dirs().sort_stats(-1).print_stats()

