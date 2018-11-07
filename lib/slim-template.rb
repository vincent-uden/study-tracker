require 'slim'
require 'ostruct'

module Slim
    VIEWS_DIR = "views"

    class Template
        def self.render(path, locals:{})
            Slim::Template.new(File.join("views", path.to_s+".slim")).render(OpenStruct.new locals )
        end
    end

    def self.sym_to_path(sym)
        sym.to_s+".slim"
    end
end

def slim(sym, locals:{}, layout: :layout)
    layout_path = File.join( Slim::VIEWS_DIR, Slim::sym_to_path(layout) )
    template_path = File.join( Slim::VIEWS_DIR, Slim::sym_to_path(sym) )
    variables = OpenStruct.new(locals)
    template = Slim::Template.new(template_path)

    if File.exist?( layout_path )
        layout = Slim::Template.new(layout_path)
        layout.render(variables) { template.render(variables) }
    else
        template.render(variables)
    end
end
