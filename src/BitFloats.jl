# * BitFloats

module BitFloats

export Float80, Float128

import BitIntegers


# * definitions

primitive type Float80  <: AbstractFloat 80  end
primitive type Float128 <: AbstractFloat 128 end

BitIntegers.@define_integers 80


end # module
