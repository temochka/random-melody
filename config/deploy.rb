lock '3.12.1'

set :deploy_to, ENV.fetch('CAPISTRANO_DEPLOY_TO')
set :keep_releases, 5
set :repo_url, 'public'
set :tarball_exclude, []

namespace :deploy do
  task :build_clean do
    run_locally do
      execute :rm, '-rf public/dist'
      execute :mkdir '-p public/dist'
    end
  end

  task build: :build_clean do
    run_locally do
      execute :elm, 'make', 'src/Main.elm', '--output', 'public/dist/elm.js', '--optimize'
    end
  end

  before :starting, :build
end
