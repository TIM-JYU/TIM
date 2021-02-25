# Run docker commands without sudo.
# Absolute path for usermod is intentional; otherwise it doesn't always work on RHEL8.
/usr/sbin/usermod -aG docker ${TIM_USER}

# Create TIM folder and adjust permissions.
mkdir -p /opt/tim
chown ${TIM_USER}:docker /opt/tim
chmod ug+rwxs /opt/tim

# Getting domain name automatically doesn't always work (e.g. because of firewalls), so we just ask it from the user.
DOMAIN=$1
# DOMAIN=$(dig +short -x $(dig +short myip.opendns.com @resolver1.opendns.com) | sed 's/.$//')

# Install TIM.
cd /opt/tim
sudo -u ${TIM_USER} git clone https://gitlab.com/tim-jyu/tim.git .
sudo -u ${TIM_USER} git submodule update --init
sudo -u ${TIM_USER} cp variables.sh.template variables.sh
chmod u+x variables.sh
sed -i 's/^echo variables.sh/#echo variables.sh/' variables.sh
SECRET_KEY=$(tr -dc A-Za-z0-9 </dev/urandom | head -c 40 ; echo '')
sudo -u ${TIM_USER} touch ./timApp/prodconfig.py
echo "SECRET_KEY = '${SECRET_KEY}'" >> ./timApp/prodconfig.py
sed -i "s|TIM_HOST=http://localhost|TIM_HOST=https://${DOMAIN}|" variables.sh
sed -i "s|CADDY_DOMAINS='http://'|CADDY_DOMAINS='${DOMAIN}'|" variables.sh
echo Pulling Docker images...
./dc pull --quiet
./npmi
./js
./up.sh

echo TIM install script finished.
