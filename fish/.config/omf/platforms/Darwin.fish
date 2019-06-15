# Darwin-specific fish config ##################################################
set -gx VAGRANT_DEFAULT_PROVIDER vmware_desktop

if type -q xcrun
  set -gx CFLAGS -I(xcrun --show-sdk-path)/usr/include
end

# Homebrew
set -gx HOMEBREW_NO_ANALYTICS 1
set -gx HOMEBREW_INSTALL_CLEANUP 1

# Link against brew openssl, zlib (Newer Python versions seem to require
# this)
set -gx LDFLAGS "-L/usr/local/lib -L/usr/local/opt/openssl/lib -L/usr/local/opt/zlib/lib"
set -gx CPPFLAGS "-I/usr/local/include -I/usr/local/opt/openssl/include -I/usr/local/opt/zlib/include"
set -gx PKG_CONFIG_PATH "/usr/local/lib/pkgconfig:/usr/local/opt/openssl/lib/pkgconfig:/usr/local/opt/zlib/lib/pkgconfig"
