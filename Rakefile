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

  Dir.mkdir(config_dir) if not Dir.exists?(config_dir) 
  
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
