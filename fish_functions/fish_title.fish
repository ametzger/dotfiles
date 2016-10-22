function fish_title --description "set the title"
	if not set -q __fish_title_hostname
		set -g __fish_title_hostname (hostname)
	end

    echo "$USER"@"$__fish_title_hostname" " | " "$_" ":" "$PWD"
end