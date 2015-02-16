module HaskellCraft.Block where

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text hiding (Show)
import           Text.Show.Text.TH (deriveShow)
import           Text.Show.Text.Data.Char

data Block = Air | Stone | Grass | Dirt | Cobblestone | Wood_planks | Sapling |
    Bedrock | Water_flowing | Water | Water_stationary | Lava_flowing | Lava |
    Lava_stationary | Sand | Gravel | Gold_ore | Iron_ore | Coal_ore | Wood |
    Leaves | Sponge | Glass | Lapis_lazuli_ore | Lapis_lazuli_block |
    Sandstone | Bed | Powered_rail | Cobweb | Grass_tall | Dead_bush | Wool |
    Flower_yellow | Flower_cyan | Mushroom_brown | Mushroom_red | Gold_block |
    Iron_block | Stone_slab_double | Stone_slab | Brick_block | Tnt |
    Bookshelf | Moss_stone | Obsidian | Torch | Fire | Stairs_wood | Chest |
    Diamond_ore | Diamond_block | Crafting_table | Seeds | Farmland |
    Furnace_inactive | Furnace_active | Sign_post | Door_wood | Ladder | Rail |
    Stairs_cobblestone | Wall_sign | Door_iron | Redstone_ore |
    Glowing_redstone_ore | Snow | Ice | Snow_block | Cactus | Clay |
    Sugar_cane | Fence | Pumpkin | Netherrack | Glowstone_block |
    Jack_o_lantern | Cake_block | Bedrock_invisible | Trapdoor | Stone_brick |
    Glass_pane | Melon | Pumpkin_stem | Melon_seeds | Fence_gate |
    Stairs_brick | Stairs_stone_brick | Nether_brick | Stairs_nether_brick |
    Stairs_sandstone | Emerald_ore | Stairs_spruce_wood | Stairs_birch_wood |
    Stairs_jungle_wood | Cobblestone_wall | Carrots | Potato | Quartz_block |
    Stairs_quartz | Wooden_double_slab | Wooden_slab | Hay_block | Carpet |
    Block_of_coal | Beetroot | Stone_cutter | Glowing_obsidian |
    Nether_reactor_core | Unknown Int
    deriving (Eq,Ord,Show)

instance T.Show Block where
    showb a = showbString $ Prelude.show a

instance Enum Block where
    toEnum 0    = Air
    toEnum 1    = Stone
    toEnum 2    = Grass
    toEnum 3    = Dirt
    toEnum 4    = Cobblestone
    toEnum 5    = Wood_planks
    toEnum 6    = Sapling
    toEnum 7    = Bedrock
    toEnum 8    = Water_flowing
    toEnum 9    = Water_stationary
    toEnum 10   = Lava_flowing
    toEnum 11   = Lava_stationary
    toEnum 12   = Sand
    toEnum 13   = Gravel
    toEnum 14   = Gold_ore
    toEnum 15   = Iron_ore
    toEnum 16   = Coal_ore
    toEnum 17   = Wood
    toEnum 18   = Leaves
    toEnum 19   = Sponge
    toEnum 20   = Glass
    toEnum 21   = Lapis_lazuli_ore
    toEnum 22   = Lapis_lazuli_block
    toEnum 24   = Sandstone
    toEnum 26   = Bed
    toEnum 27   = Powered_rail
    toEnum 30   = Cobweb
    toEnum 31   = Grass_tall
    toEnum 32   = Dead_bush
    toEnum 35   = Wool
    toEnum 37   = Flower_yellow
    toEnum 38   = Flower_cyan
    toEnum 39   = Mushroom_brown
    toEnum 40   = Mushroom_red
    toEnum 41   = Gold_block
    toEnum 42   = Iron_block
    toEnum 43   = Stone_slab_double
    toEnum 44   = Stone_slab
    toEnum 45   = Brick_block
    toEnum 46   = Tnt
    toEnum 47   = Bookshelf
    toEnum 48   = Moss_stone
    toEnum 49   = Obsidian
    toEnum 50   = Torch
    toEnum 51   = Fire
    toEnum 53   = Stairs_wood
    toEnum 54   = Chest
    toEnum 56   = Diamond_ore
    toEnum 57   = Diamond_block
    toEnum 58   = Crafting_table
    toEnum 59   = Seeds
    toEnum 60   = Farmland
    toEnum 61   = Furnace_inactive
    toEnum 62   = Furnace_active
    toEnum 63   = Sign_post
    toEnum 64   = Door_wood
    toEnum 65   = Ladder
    toEnum 66   = Rail
    toEnum 67   = Stairs_cobblestone
    toEnum 68   = Wall_sign
    toEnum 71   = Door_iron
    toEnum 73   = Redstone_ore
    toEnum 74   = Glowing_redstone_ore
    toEnum 78   = Snow
    toEnum 79   = Ice
    toEnum 80   = Snow_block
    toEnum 81   = Cactus
    toEnum 82   = Clay
    toEnum 83   = Sugar_cane
    toEnum 85   = Fence
    toEnum 86   = Pumpkin
    toEnum 87   = Netherrack
    toEnum 89   = Glowstone_block
    toEnum 91   = Jack_o_lantern
    toEnum 92   = Cake_block
    toEnum 95   = Bedrock_invisible
    toEnum 96   = Trapdoor
    toEnum 98   = Stone_brick
    toEnum 102  = Glass_pane
    toEnum 103  = Melon
    toEnum 104  = Pumpkin_stem
    toEnum 105  = Melon_seeds
    toEnum 107  = Fence_gate
    toEnum 108  = Stairs_brick
    toEnum 109  = Stairs_stone_brick
    toEnum 112  = Nether_brick
    toEnum 114  = Stairs_nether_brick
    toEnum 128  = Stairs_sandstone
    toEnum 129  = Emerald_ore
    toEnum 134  = Stairs_spruce_wood
    toEnum 135  = Stairs_birch_wood
    toEnum 136  = Stairs_jungle_wood
    toEnum 139  = Cobblestone_wall
    toEnum 141  = Carrots
    toEnum 142  = Potato
    toEnum 155  = Quartz_block
    toEnum 156  = Stairs_quartz
    toEnum 157  = Wooden_double_slab
    toEnum 158  = Wooden_slab
    toEnum 170  = Hay_block
    toEnum 171  = Carpet
    toEnum 173  = Block_of_coal
    toEnum 244  = Beetroot
    toEnum 245  = Stone_cutter
    toEnum 246  = Glowing_obsidian
    toEnum 247  = Nether_reactor_core
    toEnum i    = Unknown i

    fromEnum Air                  = 0
    fromEnum Stone                = 1
    fromEnum Grass                = 2
    fromEnum Dirt                 = 3
    fromEnum Cobblestone          = 4
    fromEnum Wood_planks          = 5
    fromEnum Sapling              = 6
    fromEnum Bedrock              = 7
    fromEnum Water_flowing        = 8
    fromEnum Water                = 8
    fromEnum Water_stationary     = 9
    fromEnum Lava_flowing         = 10
    fromEnum Lava                 = 10
    fromEnum Lava_stationary      = 11
    fromEnum Sand                 = 12
    fromEnum Gravel               = 13
    fromEnum Gold_ore             = 14
    fromEnum Iron_ore             = 15
    fromEnum Coal_ore             = 16
    fromEnum Wood                 = 17
    fromEnum Leaves               = 18
    fromEnum Sponge               = 19
    fromEnum Glass                = 20
    fromEnum Lapis_lazuli_ore     = 21
    fromEnum Lapis_lazuli_block   = 22
    fromEnum Sandstone            = 24
    fromEnum Bed                  = 26
    fromEnum Powered_rail         = 27
    fromEnum Cobweb               = 30
    fromEnum Grass_tall           = 31
    fromEnum Dead_bush            = 32
    fromEnum Wool                 = 35
    fromEnum Flower_yellow        = 37
    fromEnum Flower_cyan          = 38
    fromEnum Mushroom_brown       = 39
    fromEnum Mushroom_red         = 40
    fromEnum Gold_block           = 41
    fromEnum Iron_block           = 42
    fromEnum Stone_slab_double    = 43
    fromEnum Stone_slab           = 44
    fromEnum Brick_block          = 45
    fromEnum Tnt                  = 46
    fromEnum Bookshelf            = 47
    fromEnum Moss_stone           = 48
    fromEnum Obsidian             = 49
    fromEnum Torch                = 50
    fromEnum Fire                 = 51
    fromEnum Stairs_wood          = 53
    fromEnum Chest                = 54
    fromEnum Diamond_ore          = 56
    fromEnum Diamond_block        = 57
    fromEnum Crafting_table       = 58
    fromEnum Seeds                = 59
    fromEnum Farmland             = 60
    fromEnum Furnace_inactive     = 61
    fromEnum Furnace_active       = 62
    fromEnum Door_wood            = 64
    fromEnum Sign_post            = 63
    fromEnum Ladder               = 65
    fromEnum Rail                 = 66
    fromEnum Wall_sign            = 68
    fromEnum Stairs_cobblestone   = 67
    fromEnum Door_iron            = 71
    fromEnum Redstone_ore         = 73
    fromEnum Glowing_redstone_ore = 74
    fromEnum Snow                 = 78
    fromEnum Ice                  = 79
    fromEnum Snow_block           = 80
    fromEnum Cactus               = 81
    fromEnum Clay                 = 82
    fromEnum Sugar_cane           = 83
    fromEnum Fence                = 85
    fromEnum Pumpkin              = 86
    fromEnum Netherrack           = 87
    fromEnum Glowstone_block      = 89
    fromEnum Jack_o_lantern       = 91
    fromEnum Cake_block           = 92
    fromEnum Bedrock_invisible    = 95
    fromEnum Trapdoor             = 96
    fromEnum Stone_brick          = 98
    fromEnum Glass_pane           = 102
    fromEnum Melon                = 103
    fromEnum Pumpkin_stem         = 104
    fromEnum Melon_seeds          = 105
    fromEnum Fence_gate           = 107
    fromEnum Stairs_brick         = 108
    fromEnum Stairs_stone_brick   = 109
    fromEnum Nether_brick         = 112
    fromEnum Stairs_nether_brick  = 114
    fromEnum Stairs_sandstone     = 128
    fromEnum Emerald_ore          = 129
    fromEnum Stairs_spruce_wood   = 134
    fromEnum Stairs_birch_wood    = 135
    fromEnum Stairs_jungle_wood   = 136
    fromEnum Cobblestone_wall     = 139
    fromEnum Carrots              = 141
    fromEnum Potato               = 142
    fromEnum Quartz_block         = 155
    fromEnum Stairs_quartz        = 156
    fromEnum Wooden_double_slab   = 157
    fromEnum Wooden_slab          = 158
    fromEnum Hay_block            = 170
    fromEnum Carpet               = 171
    fromEnum Block_of_coal        = 173
    fromEnum Beetroot             = 244
    fromEnum Stone_cutter         = 245
    fromEnum Glowing_obsidian     = 246
    fromEnum Nether_reactor_core  = 247
    fromEnum (Unknown i)          = i
