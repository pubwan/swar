require 'fcntl'
require 'tk'
#require 'RSRuby'


class Array
    def radius
        squares = self.collect{|coord| coord*coord}
        sum_of_squares = squares.reduce(:+)
        Math.sqrt(sum_of_squares)
    end
    def distance(other)
    # The distance between two points, using Pythagoras' theorem.
        self.zip(other).collect{|c1, c2| c1-c2}.radius
    end
end

class R2 < Array # vector in R^2
    def initialize
    # Generate a random point that is uniformly distributed over the region inside
    # the unit circle.
        x=rand*2.0-1.0
        y=rand*2.0-1.0
        until x**2+y**2<1.0
            x=rand*2.0-1.0
            y=rand*2.0-1.0
        end
        self[0]=x
        self[1]=y
    end
    def polar_midpoint(other)
        theta1 = Math.atan2(self[1], self[0])
        theta2 = Math.atan2(other[1], other[0])
        theta = (theta1+theta2)/2.0
        theta += Math::PI if (theta1-theta2).abs > Math::PI
        r1 = Math.sqrt(self[0]*self[0]+self[1]*self[1])
        r2 = Math.sqrt(other[0]*other[0]+other[1]*other[1])
        r = (r1+r2)/2.0
        [r*Math.cos(theta), r*Math.sin(theta)]
    end
end

class Swar # swar schema object

# The Swar Schema is described at:
# http://scratchpad.wikia.com/wiki/Swar_schema
# http://geocities.ws/n8chz/swar.txt

    def self.from_file(filename) # read stream in swar markup language

    # In the Swar markup language, each non-blank line contains exactly one swar
    # ligand, which is one of "sa" (strongly attract), "wa" (weakly attract),
    # "sr" (strongly repel) or "wr" (weakly repel).

        stream = File.new(filename, "r")
        @@decode={'sa'=>2.0, 'wa'=>1.0, 'sr'=>-2.0, 'wr'=>-1.0}
        # a Swar object contains an AdjacencyHash and a LocationList
        @adjacencies=AdjacencyHash.new
        while (line = stream.gets)
            line.strip!.downcase!
            tokens = line.split(/\s+/)
            n_ligands = tokens.count{|token| @@decode.include? token}
            if n_ligands == 1 # acceptable syntax
                ligand_index = tokens.index{|token| @@decode.include? token}
                ligand = tokens[ligand_index]
                for x in 0..ligand_index-1
                    for y in ligand_index+1..tokens.count-1
                        @adjacencies.add_relation(tokens[x],ligand,tokens[y])
                    end
                end
            else #syntax error
                print "Syntax error in '#{filename}'\n"
            end
        end
        @locations = @adjacencies.make_location_list
        Swar.new(@adjacencies, @locations)
    end

    def initialize(adjacencies = AdjacencyHash.new, locations = adjacencies.make_location_list)
        @adjacencies = adjacencies
        @locations = locations
    end

    def adjacencies
    # A hash which represents the upper triangle of the adjacency matrix
    # describing the graph described by the swar schema file.
    # Example: If 'a sa b' in the swar schema file from which Swar instance
    # 'swar' is instantiated, swar["a"]["b"] => 2.0. (The numeric value of "sa"
    # is 2.0.)
        @adjacencies
    end

    def locations
    # A hash which gives the location of each node.
        @locations
    end

    def nodes
    # An array containing each of the node names in the Swar instance.
        @locations.keys
    end

    def energy
        dt = @locations.distance_table
        distances = dt.collect{|x, y| y}
        node_pairs = dt.collect{|x, y| x}
        adjacencies = node_pairs.collect{|x, y| @adjacencies.adjacency(x, y)}
        distances.zip(adjacencies).collect{|x, y| x*y}.reduce(:+)
    end

    def draw
    end

end

class AdjacencyHash < Hash

    @@decode={'sa'=>2.0, 'wa'=>1.0, 'sr'=>-2.0, 'wr'=>-1.0}

    def initialize
        self.default=0
    end

    def add_relation(x, ligand, y)
        return self.add_relation(y, ligand, x) if x>y
        self[x] = AdjacencyHash.new if self[x] == 0
        print "relationship between '#{x}' and '#{y}' redefined\n" \
            if self[x][y] != 0
        self[x][y]=@@decode[ligand]
        return self
    end

    def adjacency(x, y)
        return self.adjacency(y, x) if x>y
        return 0 if self[x]==0
        return self[x][y]
    end

    def node_list
        self.values.collect{|hash| hash.keys}.push(self.keys).flatten.uniq.sort
    end

    def make_location_list
        LocationList.new self.node_list
    end

end


class LocationList < Hash

    def initialize(node_list)
        node_list.each {|node| self[node]=R2.new}
    end

    def distance_table
        self.node_pairs.collect{|p1, p2| [[p1, p2], self[p1].distance(self[p2])]}

        #node_pairs.collect{|pair| [pair, distance(self[pair[0]], self[pair[1]])]}
    end

    def node_pairs

        # Make a list of left-nodes:
        nodes = self.keys

        # Associate each left-node with its list of right-nodes:
        groups = nodes.collect{|node| [node,nodes[nodes.index(node)+1..-1]]}

        # Make list of each pairing of a left-node with a right-node
        groups.collect{|x| x[1].collect{|y| [x[0],y]}}.flatten(1)

    end

    def location_of(node)
         self[node]
    end

    def flip
    # Create a copy of self in which each node's x-coordinate is replaced with
    # its additive inverse; creating a mirror image of the underlying graph.

        return_value = Hash.new(0)
        self.each{|key, value| return_value[key]=[-value[0], value[1]]}
        return_value
    end

    def normalize!
        vals = self.values
        sums = vals.transpose.collect{|coords| coords.reduce(:+)}
        centroid = sums.collect{|sum| sum/self.count}
        shifted = vals.collect{|pt| [pt[0]-centroid[0], pt[1]-centroid[1]]}
        scale = shifted.collect{|pt| pt.radius}.max
        #scale = shifted.collect{|pt| distance([0, 0], pt)}.max
        self.keys.collect{|key| self[key]=self[key].collect{|c| c/scale}}
    end

end

