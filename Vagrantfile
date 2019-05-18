# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/bionic64"

  config.vm.network "forwarded_port", guest: 8080, host: 8080, host_ip: "127.0.0.1"

  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get install -y erlang make git docker.io docker-compose python3-pip
    pip3 install git+https://github.com/domenukk/enochecker

    apt-get install -y httpie

    sed -i -e 's|^#\(force_color_prompt\)|\1|' /root/.bashrc
  SHELL
end
