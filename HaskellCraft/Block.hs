module HaskellCraft.Block where

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text hiding (Show)
import           Text.Show.Text.TH (deriveShow)
import           Text.Show.Text.Data.Char

data Block = Air | Stone | Grass | Dirt | Cobblestone | Wood_planks | Sapling |
    Bedrock | Water_flowing | Water | Water_stationary | Lava_flowing | Lava |
    Lava_stationary | Sand | Gravel | Gold_ore | Iron_ore | Coal_ore | Wood |
    Leaves | Glass | Lapis_lazuli_ore | Lapis_lazuli_block | Sandstone | Bed |
    Cobweb | Grass_tall | Wool | Flower_yellow | Flower_cyan | Mushroom_brown |
    Mushroom_red | Gold_block | Iron_block | Stone_slab_double | Stone_slab |
    Brick_block | Tnt | Bookshelf | Moss_stone | Obsidian | Torch | Fire |
    Stairs_wood | Chest | Diamond_ore | Diamond_block | Crafting_table |
    Farmland | Furnace_inactive | Furnace_active | Door_wood | Ladder |
    Stairs_cobblestone | Door_iron | Redstone_ore | Snow | Ice | Snow_block |
    Cactus | Clay | Sugar_cane | Fence | Netherrack | Glowstone_block |
    Bedrock_invisible | Trapdoor | Stone_brick | Glass_pane | Melon |
    Melon_seeds | Fence_gate | Stairs_brick | Stairs_stone_brick |
    Nether_brick | Stairs_nether_brick | Stairs_sandstone | Quartz_block |
    Stairs_quartz | Stone_cutter | Glowing_obsidian | Nether_reactor_core |
    Unknown Int
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
    toEnum 20   = Glass
    toEnum 21   = Lapis_lazuli_ore
    toEnum 22   = Lapis_lazuli_block
    toEnum 24   = Sandstone
    toEnum 26   = Bed
    toEnum 30   = Cobweb
    toEnum 31   = Grass_tall
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
    toEnum 60   = Farmland
    toEnum 61   = Furnace_inactive
    toEnum 62   = Furnace_active
    toEnum 64   = Door_wood
    toEnum 65   = Ladder
    toEnum 67   = Stairs_cobblestone
    toEnum 71   = Door_iron
    toEnum 73   = Redstone_ore
    toEnum 78   = Snow
    toEnum 79   = Ice
    toEnum 80   = Snow_block
    toEnum 81   = Cactus
    toEnum 82   = Clay
    toEnum 83   = Sugar_cane
    toEnum 85   = Fence
    toEnum 87   = Netherrack
    toEnum 89   = Glowstone_block
    toEnum 95   = Bedrock_invisible
    toEnum 96   = Trapdoor
    toEnum 98   = Stone_brick
    toEnum 102  = Glass_pane
    toEnum 103  = Melon
    toEnum 105  = Melon_seeds
    toEnum 107  = Fence_gate
    toEnum 108  = Stairs_brick
    toEnum 109  = Stairs_stone_brick
    toEnum 112  = Nether_brick
    toEnum 114  = Stairs_nether_brick
    toEnum 128  = Stairs_sandstone
    toEnum 155  = Quartz_block
    toEnum 156  = Stairs_quartz
    toEnum 245  = Stone_cutter
    toEnum 246  = Glowing_obsidian
    toEnum 247  = Nether_reactor_core
    toEnum i    = Unknown i

    fromEnum Air                 = 0
    fromEnum Stone               = 1
    fromEnum Grass               = 2
    fromEnum Dirt                = 3
    fromEnum Cobblestone         = 4
    fromEnum Wood_planks         = 5
    fromEnum Sapling             = 6
    fromEnum Bedrock             = 7
    fromEnum Water_flowing       = 8
    fromEnum Water               = 8
    fromEnum Water_stationary    = 9
    fromEnum Lava_flowing        = 10
    fromEnum Lava                = 10
    fromEnum Lava_stationary     = 11
    fromEnum Sand                = 12
    fromEnum Gravel              = 13
    fromEnum Gold_ore            = 14
    fromEnum Iron_ore            = 15
    fromEnum Coal_ore            = 16
    fromEnum Wood                = 17
    fromEnum Leaves              = 18
    fromEnum Glass               = 20
    fromEnum Lapis_lazuli_ore    = 21
    fromEnum Lapis_lazuli_block  = 22
    fromEnum Sandstone           = 24
    fromEnum Bed                 = 26
    fromEnum Cobweb              = 30
    fromEnum Grass_tall          = 31
    fromEnum Wool                = 35
    fromEnum Flower_yellow       = 37
    fromEnum Flower_cyan         = 38
    fromEnum Mushroom_brown      = 39
    fromEnum Mushroom_red        = 40
    fromEnum Gold_block          = 41
    fromEnum Iron_block          = 42
    fromEnum Stone_slab_double   = 43
    fromEnum Stone_slab          = 44
    fromEnum Brick_block         = 45
    fromEnum Tnt                 = 46
    fromEnum Bookshelf           = 47
    fromEnum Moss_stone          = 48
    fromEnum Obsidian            = 49
    fromEnum Torch               = 50
    fromEnum Fire                = 51
    fromEnum Stairs_wood         = 53
    fromEnum Chest               = 54
    fromEnum Diamond_ore         = 56
    fromEnum Diamond_block       = 57
    fromEnum Crafting_table      = 58
    fromEnum Farmland            = 60
    fromEnum Furnace_inactive    = 61
    fromEnum Furnace_active      = 62
    fromEnum Door_wood           = 64
    fromEnum Ladder              = 65
    fromEnum Stairs_cobblestone  = 67
    fromEnum Door_iron           = 71
    fromEnum Redstone_ore        = 73
    fromEnum Snow                = 78
    fromEnum Ice                 = 79
    fromEnum Snow_block          = 80
    fromEnum Cactus              = 81
    fromEnum Clay                = 82
    fromEnum Sugar_cane          = 83
    fromEnum Fence               = 85
    fromEnum Netherrack          = 87
    fromEnum Glowstone_block     = 89
    fromEnum Bedrock_invisible   = 95
    fromEnum Trapdoor            = 96
    fromEnum Stone_brick         = 98
    fromEnum Glass_pane          = 102
    fromEnum Melon               = 103
    fromEnum Melon_seeds         = 105
    fromEnum Fence_gate          = 107
    fromEnum Stairs_brick        = 108
    fromEnum Stairs_stone_brick  = 109
    fromEnum Nether_brick        = 112
    fromEnum Stairs_nether_brick = 114
    fromEnum Stairs_sandstone    = 128
    fromEnum Quartz_block        = 155
    fromEnum Stairs_quartz       = 156
    fromEnum Stone_cutter        = 245
    fromEnum Glowing_obsidian    = 246
    fromEnum Nether_reactor_core = 247
    fromEnum (Unknown i)         = i
