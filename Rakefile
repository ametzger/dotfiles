# lifted from https://github.com/defeated/dotfiles/blob/master/rakefile

task :default do
  puts `rake -T`
end

desc "Install symlinks from #{Dir.pwd}/dotfiles to ~/"
task :install do
  home = Dir.home

  puts "installing dotfiles into #{home}"
  
  # make symlinks for everything except ~/.config
  Dir["dotfiles/*"].reject { |p| p.end_with? "/config" }.each do |f| make_symlink(home, f, ".") end

  config_dir = File.join(home, ".config")  
  fish_config_path = File.join(config_dir, "fish")
  Dir.mkdir(fish_config_path) unless File.exists? fish_config_path 
  
  Dir["dotfiles/config/fish/*"].each do |p|
    make_symlink(config_dir + "/fish", p, nil)
  end
  
end

def make_symlink(dest_base, f, prefix)
  name  = File.basename(f)
  src   = File.join(Dir.pwd, f)
  dest  = File.join(dest_base, "#{prefix}#{name}")
  
  return if File.exists?(dest)

  puts "linking #{src} \t => #{dest}"

  FileUtils.ln_sf(src, dest)
end

def make_directory_if_not_exist(path)
  segment = ""
  path.split("/").each { |d|
    segment += d
    Dir.mkdir segment unless Dir.exists?(segment)
    segment += '/'
  }
end
