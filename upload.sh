#!/bin/bash
rm -f .stack-work/install/x86_64-linux/lts-12.2/8.4.3/bin/some-paste-exe-packed
upx -9 .stack-work/install/x86_64-linux/lts-12.2/8.4.3/bin/some-paste-exe -o.stack-work/install/x86_64-linux/lts-12.2/8.4.3/bin/some-paste-exe-packed
ssh vps "mv some-paste/some-paste some-paste/some-paste.old"
scp .stack-work/install/x86_64-linux/lts-12.2/8.4.3/bin/some-paste-exe-packed vps:some-paste/some-paste
ssh vps "sudo systemctl restart somepaste"
