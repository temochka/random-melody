server ENV.fetch('CAPISTRANO_SERVER'), user: 'deploy', roles: %w(web), port: ENV.fetch('CAPISTRANO_PORT')
