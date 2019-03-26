# lifted from https://github.com/defeated/dotfiles/blob/master/rakefile

task :default do
  puts `rake -T`
end

desc "Install symlinks from #{Dir.pwd}/dotfiles to ~/"
task :install do
  home = Dir.home

  puts "installing dotfiles into #{home}"

  # make symlinks for everything except ~/.config
  Dir['dotfiles/*'].reject { |p| p.end_with? '/config' }.each do |f|
    make_symlink(home, f, '.')
  end

  config_dir = File.join(home, '.config')
  df_config_dir = 'dotfiles/config'

  Dir["#{df_config_dir}/**/*"].reject { |p| File.directory? p }.each do |p|
    dir_path = File.dirname(p)
    dir_path.slice! 'dotfiles/config/'

    target_dir = File.join(config_dir, dir_path)

    `mkdir -p #{target_dir}`
    make_symlink(target_dir, p, nil)
  end
end

def make_symlink(dest_base, fname, prefix = '')
  name  = File.basename(fname)
  src   = File.join(Dir.pwd, fname)
  dest  = File.join(dest_base, "#{prefix}#{name}")

  return if File.exist?(dest)

  puts "linking #{dest} \t => #{src}"

  FileUtils.ln_sf(src, dest)
end
