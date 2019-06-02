# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/disco64"

  config.vm.network "forwarded_port", guest: 8080, host: 8080, host_ip: "127.0.0.1"

  config.vm.synced_folder ".", "/vagrant", type: "rsync"

  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get install -y erlang make git docker.io python3-pip
    pip3 install -r checker/requirements.txt

    apt-get install -y httpie gdebi
    wget https://github.com/vi/websocat/releases/download/v1.4.0/websocat_1.4.0_ssl1.1_amd64.deb
    gdebi --non-interactive websocat_1.4.0_ssl1.1_amd64.deb

    sed -i -e 's/^#force_color_prompt/force_color_prompt/' /root/.bashrc

    curl -L "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
    chmod +x /usr/local/bin/docker-compose

    wget https://download.virtualbox.org/virtualbox/5.2.30/VBoxGuestAdditions_5.2.30.iso -P /tmp
    mount -o loop /tmp/VBoxGuestAdditions_5.2.30.iso /mnt
    echo yes | sh /mnt/VBoxLinuxAdditions.run
  SHELL
end
