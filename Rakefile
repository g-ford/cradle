require "rubygems"
require 'rake'
require 'yaml'
require 'time'
require 'open-uri'
#require "RMagick"

SOURCE = "."
CONFIG = {
  'layouts' => File.join(SOURCE, "_layouts"),
  'posts' => File.join(SOURCE, "_posts")
}

###
# Based on jekyll-bootstrap's Rakefile.
# Thanks, @plusjade
# https://github.com/plusjade/jekyll-bootstrap
###

# Usage: rake post title="A Title" [date="2012-02-09"]
desc "Begin a new post in #{CONFIG['posts']}"
task :post do
  abort("rake aborted: '#{CONFIG['posts']}' directory not found.") unless FileTest.directory?(CONFIG['posts'])
  title = ENV["title"] || "new-post"
  slug = title.downcase.strip.gsub(' ', '-').gsub(/[^\w-]/, '')
  begin
    date = (ENV['date'] ? Time.parse(ENV['date']) : Time.now).strftime('%Y-%m-%d')
  rescue Exception => e
    puts "Error - date format must be YYYY-MM-DD, please check you typed it correctly!"
    exit -1
  end
  filename = File.join(CONFIG['posts'], "#{date}-#{slug}.md")
  if File.exist?(filename)
    abort("rake aborted!") if ask("#{filename} already exists. Do you want to overwrite?", ['y', 'n']) == 'n'
  end

  puts "Creating new post: #{filename}"
  open(filename, 'w') do |post|
    post.puts "---"
    post.puts "layout: post"
    post.puts "title: \"#{title.gsub(/-/,' ')}\""
    post.puts "category: posts"
    post.puts "---"
  end
end # task :post

desc "Launch preview environment"
task :preview do
  system "jekyll --auto --server"
end # task :preview

desc "Update icons based on gravatar!"
task :icons do
  gravatar_id = "7ef3cd04888c6d17f34d43e29ec3d46f"
  base_url = "https://secure.gravatar.com/avatar/#{gravatar_id}?s=500"

  origin = "origin.png"
  File.delete origin if File.exist? origin

  open(origin, 'wb') do |file|
    file << open(base_url).read
  end

  name_pre = "apple-touch-icon-%dx%d-precomposed.png"

  FileList["*apple-touch-ico*.png"].each do |img|
    File.delete img
  end

  FileList["*favicon.ico"].each do |img|
    File.delete img
  end

  puts "creating favicon.ico"
  Magick::Image::read(origin).first.resize(16, 16).write("favicon.ico")

  [144, 114, 72, 57].each do |size|
    puts "creating %d * %d icons" % [size, size]
    Magick::Image::read(origin).first.resize(size, size).write(name_pre % [size, size])
  end

  File.delete origin
end


