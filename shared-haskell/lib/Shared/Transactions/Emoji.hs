module Shared.Transactions.Emoji where

import           Data.Text                      ( Text )

-- brittany-disable-next-binding
convertMCCToEmoji :: Int -> Text
convertMCCToEmoji mcc
  | mcc == 742 = "&#x1F436;" -- Veterinary Services
  | mcc == 763 = "&#x1F951;" -- Agricultural Cooperative
  | mcc == 780 = "&#x1F333;" -- Landscaping Services
  | mcc == 1520 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- General Contractors
  | mcc == 1711 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Heating, Plumbing, A/C
  | mcc == 1731 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Electrical Contractors
  | mcc == 1740 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Masonry, Stonework, and Plaster
  | mcc == 1750 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Carpentry Contractors
  | mcc == 1761 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Roofing/Siding, Sheet Metal
  | mcc == 1771 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Concrete Work Contractors
  | mcc == 1799 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Special Trade Contractors
  | mcc == 2741 = "&#x1F5A8;" -- Miscellaneous Publishing and Printing
  | mcc == 2791 = "&#x1F5A8;" -- Typesetting, Plate Making, and Related Services
  | mcc == 2842 = "&#x1F9F9;" -- Specialty Cleaning
  | 3000 <= mcc && mcc <= 3299 = "&#x2708;&#xFE0F;" -- Airlines
  | 3351 <= mcc && mcc <= 3441 = "&#x1F698;" -- Car Rental
  | 3501 <= mcc && mcc <= 3790 = "&#x1F3D6;" -- Hotels/Motels/Inns/Resorts
  | mcc == 4011 = "&#x1F682;" -- Railroads
  | mcc == 4111 = "&#x1F6A2;" -- Commuter Transport, Ferries
  | mcc == 4112 = "&#x1F682;" -- Passenger Railways
  | mcc == 4119 = "&#x1F691;" -- Ambulance Services
  | mcc == 4121 = "&#x1F695;" -- Taxicabs/Limousines
  | mcc == 4131 = "&#x1F68D;" -- Bus Lines
  | mcc == 4214 = "&#x1F4E6;" -- Motor Freight Carriers and Trucking - Local and Long Distance, Moving and Storage Companies, and Local Delivery Services
  | mcc == 4215 = "&#x1F4E6;" -- Courier Services
  | mcc == 4225 = "&#x1F4E6;" -- Public Warehousing and Storage - Farm Products, Refrigerated Goods, Household Goods, and Storage
  | mcc == 4411 = "&#x1F6F3;" -- Cruise Lines
  | mcc == 4457 = "&#x1F6A4;" -- Boat Rentals and Leases
  | mcc == 4468 = "&#x2693;&#xFE0F;" -- Marinas, Service and Supplies
  | mcc == 4511 = "&#x2708;&#xFE0F;" -- Airlines, Air Carriers
  | mcc == 4582 = "&#x2708;&#xFE0F;" -- Airports, Flying Fields
  | mcc == 4722 = "&#x2708;&#xFE0F;" -- Travel Agencies, Tour Operators
  | mcc == 4723 = "&#x2708;&#xFE0F;" -- TUI Travel - Germany
  | mcc == 4784 = "&#x1F698;" -- Tolls/Bridge Fees
  | mcc == 4789 = "&#x1F698;" -- Transportation Services (Not Elsewhere Classified)
  | mcc == 4812 = "&#x260E;&#xFE0F;" -- Telecommunication Equipment and Telephone Sales
  | mcc == 4814 = "&#x260E;&#xFE0F;" -- Telecommunication Services
  | mcc == 4816 = "&#x1F469;&#x200D;&#x1F4BB;" -- Computer Network Services
  | mcc == 4821 = "&#x260E;&#xFE0F;" -- Telegraph Services
  | mcc == 4829 = "&#x1F4B8;" -- Wires, Money Orders
  | mcc == 4899 = "&#x1F4FA;" -- Cable, Satellite, and Other Pay Television and Radio
  | mcc == 4900 = "&#x26A1;&#xFE0F;" -- Utilities
  | mcc == 5013 = "&#x1F698;" -- Motor Vehicle Supplies and New Parts
  | mcc == 5021 = "&#x1FA91;" -- Office and Commercial Furniture
  | mcc == 5039 = "&#x1F6A7;" -- Construction Materials (Not Elsewhere Classified)
  | mcc == 5044 = "&#x1F4F8;" -- Photographic, Photocopy, Microfilm Equipment, and Supplies
  | mcc == 5045 = "&#x1F4BB;" -- Computers, Peripherals, and Software
  | mcc == 5046 = "&#x1F6E0;" -- Commercial Equipment (Not Elsewhere Classified)
  | mcc == 5047 = "&#x1FA7A;" -- Medical, Dental, Ophthalmic, and Hospital Equipment and Supplies
  | mcc == 5051 = "&#x1FA7A;" -- Metal Service Centers
  | mcc == 5065 = "&#x1F6E0;" -- Electrical Parts and Equipment
  | mcc == 5072 = "&#x1F6E0;" -- Hardware, Equipment, and Supplies
  | mcc == 5074 = "&#x1F3D7;" -- Plumbing, Heating Equipment, and Supplies
  | mcc == 5085 = "&#x1F3D7;" -- Industrial Supplies (Not Elsewhere Classified)
  | mcc == 5094 = "&#x1F48E;" -- Precious Stones and Metals, Watches and Jewelry
  | mcc == 5099 = "&#x1F4B3;" -- Durable Goods (Not Elsewhere Classified)
  | mcc == 5111 = "&#x1F4CE;" -- Stationary, Office Supplies, Printing and Writing Paper
  | mcc == 5122 = "&#x1F48A;" -- Drugs, Drug Proprietaries, and Druggist Sundries
  | mcc == 5131 = "&#x1F4B3;" -- Piece Goods, Notions, and Other Dry Goods
  | mcc == 5137 = "&#x1F9BA;" -- Uniforms, Commercial Clothing
  | mcc == 5139 = "&#x1F97E;" -- Commercial Footwear
  | mcc == 5169 = "&#x1F9EA;" -- Chemicals and Allied Products (Not Elsewhere Classified)
  | mcc == 5172 = "&#x26FD;&#xFE0F;" -- Petroleum and Petroleum Products
  | mcc == 5192 = "&#x1F4DA;" -- Books, Periodicals, and Newspapers
  | mcc == 5193 = "&#x1F490;" -- Florists Supplies, Nursery Stock, and Flowers
  | mcc == 5198 = "&#x1F3A8;" -- Paints, Varnishes, and Supplies
  | mcc == 5199 = "&#x1F4B3;" -- Nondurable Goods (Not Elsewhere Classified)
  | mcc == 5200 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Home Supply Warehouse Stores
  | mcc == 5211 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Lumber, Building Materials Stores
  | mcc == 5231 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Glass, Paint, and Wallpaper Stores
  | mcc == 5251 = "&#x1F477;&#x200D;&#x2640;&#xFE0F;" -- Hardware Stores
  | mcc == 5261 = "&#x1F333;" -- Nurseries, Lawn and Garden Supply Stores
  | mcc == 5271 = "&#x1F3E0;" -- Mobile Home Dealers
  | mcc == 5300 = "&#x1F6CD;" -- Wholesale Clubs
  | mcc == 5309 = "&#x1F6CD;" -- Duty Free Stores
  | mcc == 5310 = "&#x1F6CD;" -- Discount Stores
  | mcc == 5311 = "&#x1F6CD;" -- Department Stores
  | mcc == 5331 = "&#x1F6CD;" -- Variety Stores
  | mcc == 5399 = "&#x1F6CD;" -- Miscellaneous General Merchandise
  | mcc == 5411 = "&#x1F951;" -- Grocery Stores, Supermarkets
  | mcc == 5422 = "&#x1F969;" -- Freezer and Locker Meat Provisioners
  | mcc == 5441 = "&#x1F36C;" -- Candy, Nut, and Confectionery Stores
  | mcc == 5451 = "&#x1F42E;" -- Dairy Products Stores
  | mcc == 5462 = "&#x1F950;" -- Bakeries
  | mcc == 5499 = "&#x1F951;" -- Miscellaneous Food Stores - Convenience Stores and Specialty Markets
  | mcc == 5511 = "&#x1F698;" -- Car and Truck Dealers (New & Used) Sales, Service, Repairs Parts and Leasing
  | mcc == 5521 = "&#x1F698;" -- Car and Truck Dealers (Used Only) Sales, Service, Repairs Parts and Leasing
  | mcc == 5531 = "&#x1F698;" -- Auto and Home Supply Stores
  | mcc == 5532 = "&#x1F698;" -- Automotive Tire Stores
  | mcc == 5533 = "&#x1F698;" -- Automotive Parts and Accessories Stores
  | mcc == 5541 = "&#x1F698;" -- Service Stations
  | mcc == 5542 = "&#x26FD;&#xFE0F;" -- Automated Fuel Dispensers
  | mcc == 5551 = "&#x2693;&#xFE0F;" -- Boat Dealers
  | mcc == 5561 = "&#x1F6F5;" -- Motorcycle Shops, Dealers
  | mcc == 5571 = "&#x1F6F5;" -- Motorcycle Shops and Dealers
  | mcc == 5592 = "&#x1F3E0;" -- Motor Homes Dealers
  | mcc == 5598 = "&#x2744;&#xFE0F;" -- Snowmobile Dealers
  | mcc == 5599 = "&#x1F698;" -- Miscellaneous Auto Dealers
  | mcc == 5611 = "&#x1F455;" -- Men&#x27;s and Boy&#x27;s Clothing and Accessories Stores
  | mcc == 5621 = "&#x1F457;" -- Women&#x27;s Ready-To-Wear Stores
  | mcc == 5631 = "&#x1F45C;" -- Women&#x27;s Accessory and Specialty Shops
  | mcc == 5641 = "&#x1F423;" -- Children&#x27;s and Infant&#x27;s Wear Stores
  | mcc == 5651 = "&#x1F455;" -- Family Clothing Stores
  | mcc == 5655 = "&#x1F3BD;" -- Sports and Riding Apparel Stores
  | mcc == 5661 = "&#x1F460;" -- Shoe Stores
  | mcc == 5681 = "&#x1F4B3;" -- Furriers and Fur Shops
  | mcc == 5691 = "&#x1F455;" -- Men&#x27;s, Women&#x27;s Clothing Stores
  | mcc == 5697 = "&#x2702;&#xFE0F;" -- Tailors, Alterations
  | mcc == 5698 = "&#x1F4B3;" -- Wig and Toupee Stores
  | mcc == 5699 = "&#x1F45C;" -- Miscellaneous Apparel and Accessory Shops
  | mcc == 5712 = "&#x1FA91;" -- Furniture, Home Furnishings, and Equipment Stores, Except Appliances
  | mcc == 5713 = "&#x1F4B3;" -- Floor Covering Stores
  | mcc == 5714 = "&#x1F3E0;" -- Drapery, Window Covering, and Upholstery Stores
  | mcc == 5718 = "&#x1F3E0;" -- Fireplace, Fireplace Screens, and Accessories Stores
  | mcc == 5719 = "&#x1FA91;" -- Miscellaneous Home Furnishing Specialty Stores
  | mcc == 5722 = "&#x1F50C;" -- Household Appliance Stores
  | mcc == 5732 = "&#x1F50C;" -- Electronics Stores
  | mcc == 5733 = "&#x1F468;&#x200D;&#x1F3A4;" -- Music Stores-Musical Instruments, Pianos, and Sheet Music
  | mcc == 5734 = "&#x1F469;&#x200D;&#x1F4BB;" -- Computer Software Stores
  | mcc == 5735 = "&#x1F3B5;" -- Record Stores
  | mcc == 5811 = "&#x1F469;&#x200D;&#x1F373;" -- Caterers
  | mcc == 5812 = "&#x1F37D;" -- Eating Places, Restaurants
  | mcc == 5813 = "&#x1F37B;" -- Drinking Places
  | mcc == 5814 = "&#x1F37D;" -- Fast Food Restaurants
  | mcc == 5912 = "&#x1F48A;" -- Drug Stores and Pharmacies
  | mcc == 5921 = "&#x1F37B;" -- Package Stores-Beer, Wine, and Liquor
  | mcc == 5931 = "&#x1F4B3;" -- Used Merchandise and Secondhand Stores
  | mcc == 5932 = "&#x1F4B3;" -- Antique Shops
  | mcc == 5933 = "&#x1F4B3;" -- Pawn Shops
  | mcc == 5935 = "&#x1F4B3;" -- Wrecking and Salvage Yards
  | mcc == 5937 = "&#x1F4B3;" -- Antique Reproductions
  | mcc == 5940 = "&#x1F6B2;" -- Bicycle Shops
  | mcc == 5941 = "&#x1F3D3;" -- Sporting Goods Stores
  | mcc == 5942 = "&#x1F4DA;" -- Book Stores
  | mcc == 5943 = "&#x1F4CE;" -- Stationery Stores, Office, and School Supply Stores
  | mcc == 5944 = "&#x231A;&#xFE0F;" -- Jewelry Stores, Watches, Clocks, and Silverware Stores
  | mcc == 5945 = "&#x1FA80;" -- Hobby, Toy, and Game Shops
  | mcc == 5946 = "&#x1F4F8;" -- Camera and Photographic Supply Stores
  | mcc == 5947 = "&#x1F381;" -- Gift, Card, Novelty, and Souvenir Shops
  | mcc == 5948 = "&#x1F4BC;" -- Luggage and Leather Goods Stores
  | mcc == 5949 = "&#x1F9F5;" -- Sewing, Needlework, Fabric, and Piece Goods Stores
  | mcc == 5950 = "&#x1F942;" -- Glassware, Crystal Stores
  | mcc == 5960 = "&#x1F4B3;" -- Direct Marketing - Insurance Services
  | mcc == 5962 = "&#x1F4B3;" -- Direct Marketing - Travel
  | mcc == 5963 = "&#x1F4B3;" -- Door-To-Door Sales
  | mcc == 5964 = "&#x1F4B3;" -- Direct Marketing - Catalog Merchant
  | mcc == 5965 = "&#x1F4B3;" -- Direct Marketing - Combination Catalog and Retail Merchant
  | mcc == 5966 = "&#x1F4B3;" -- Direct Marketing - Outbound Tele
  | mcc == 5967 = "&#x1F4B3;" -- Direct Marketing - Inbound Tele
  | mcc == 5968 = "&#x1F4B3;" -- Direct Marketing - Subscription
  | mcc == 5969 = "&#x1F4B3;" -- Direct Marketing - Other
  | mcc == 5970 = "&#x1F3A8;" -- Artist&#x27;s Supply and Craft Shops
  | mcc == 5971 = "&#x1F3A8;" -- Art Dealers and Galleries
  | mcc == 5972 = "&#x1F4B3;" -- Stamp and Coin Stores
  | mcc == 5973 = "&#x1F4B3;" -- Religious Goods Stores
  | mcc == 5975 = "&#x1F4B3;" -- Hearing Aids Sales and Supplies
  | mcc == 5976 = "&#x1F9BE;" -- Orthopedic Goods - Prosthetic Devices
  | mcc == 5977 = "&#x1F484;" -- Cosmetic Stores
  | mcc == 5978 = "&#x2328;&#xFE0F;" -- Typewriter Stores
  | mcc == 5983 = "&#x26FD;&#xFE0F;" -- Fuel Dealers (Non Automotive)
  | mcc == 5992 = "&#x1F490;" -- Florists
  | mcc == 5993 = "&#x1F6AC;" -- Cigar Stores and Stands
  | mcc == 5994 = "&#x1F5DE;" -- News Dealers and Newsstands
  | mcc == 5995 = "&#x1F436;" -- Pet Shops, Pet Food, and Supplies
  | mcc == 5996 = "&#x1F3CA;&#x200D;&#x2640;&#xFE0F;" -- Swimming Pools Sales
  | mcc == 5997 = "&#x1FA92;" -- Electric Razor Stores
  | mcc == 5998 = "&#x26FA;&#xFE0F;" -- Tent and Awning Shops
  | mcc == 5999 = "&#x1F4B3;" -- Miscellaneous Specialty Retail
  | mcc == 6010 = "&#x1F4B3;" -- Manual Cash Disburse
  | mcc == 6011 = "&#x1F4B3;" -- Automated Cash Disburse
  | mcc == 6012 = "&#x1F4B3;" -- Financial Institutions
  | mcc == 6051 = "&#x1F4B3;" -- Non-FI, Money Orders
  | mcc == 6211 = "&#x1F4B3;" -- Security Brokers/Dealers
  | mcc == 6300 = "&#x1F4B3;" -- Insurance Underwriting, Premiums
  | mcc == 6399 = "&#x1F4B3;" -- Insurance - Default
  | mcc == 6513 = "&#x1F4B3;" -- Real Estate Agents and Managers - Rentals
  | mcc == 7011 = "&#x1F3D6;" -- Hotels, Motels, and Resorts
  | mcc == 7012 = "&#x1F3D6;" -- Timeshares
  | mcc == 7032 = "&#x1F3D5;" -- Sporting/Recreation Camps
  | mcc == 7033 = "&#x1F3D5;" -- Trailer Parks, Campgrounds
  | mcc == 7210 = "&#x1F9F9;" -- Laundry, Cleaning Services
  | mcc == 7211 = "&#x1F9F9;" -- Laundries
  | mcc == 7216 = "&#x1F9FA;" -- Dry Cleaners
  | mcc == 7217 = "&#x1F9F9;" -- Carpet/Upholstery Cleaning
  | mcc == 7221 = "&#x1F933;" -- Photographic Studios
  | mcc == 7230 = "&#x1F488;" -- Barber and Beauty Shops
  | mcc == 7251 = "&#x1F45E;" -- Shoe Repair/Hat Cleaning
  | mcc == 7261 = "&#x1F4B3;" -- Funeral Services, Crematories
  | mcc == 7273 = "&#x1F346;" -- Dating/Escort Services
  | mcc == 7276 = "&#x1F4B8;" -- Tax Preparation Services
  | mcc == 7277 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Counseling Services
  | mcc == 7278 = "&#x1F6CD;" -- Buying/Shopping Services
  | mcc == 7296 = "&#x1F457;" -- Clothing Rental
  | mcc == 7297 = "&#x1F486;&#x200D;&#x2640;&#xFE0F;" -- Massage Parlors
  | mcc == 7298 = "&#x1F9D6;&#x200D;&#x2640;&#xFE0F;" -- Health and Beauty Spas
  | mcc == 7299 = "&#x1F4B3;" -- Miscellaneous General Services
  | mcc == 7311 = "&#x1F4B3;" -- Advertising Services
  | mcc == 7321 = "&#x1F4B3;" -- Credit Reporting Agencies
  | mcc == 7333 = "&#x1F469;&#x200D;&#x1F3A8;" -- Commercial Photography, Art and Graphics
  | mcc == 7338 = "&#x1F5A8;" -- Quick Copy, Repro, and Blueprint
  | mcc == 7339 = "&#x1F468;&#x200D;&#x1F4BC;" -- Secretarial Support Services
  | mcc == 7342 = "&#x1F400;" -- Exterminating Services
  | mcc == 7349 = "&#x1F9F9;" -- Cleaning and Maintenance
  | mcc == 7361 = "&#x1F4B3;" -- Employment/Temp Agencies
  | mcc == 7372 = "&#x1F469;&#x200D;&#x1F4BB;" -- Computer Programming
  | mcc == 7375 = "&#x1F575;&#xFE0F;&#x200D;&#x2640;&#xFE0F;" -- Information Retrieval Services
  | mcc == 7379 = "&#x1F469;&#x200D;&#x1F4BB;" -- Computer Repair
  | mcc == 7392 = "&#x1F469;&#x200D;&#x1F4BC;" -- Consulting, Public Relations
  | mcc == 7393 = "&#x1F575;&#xFE0F;&#x200D;&#x2640;&#xFE0F;" -- Detective Agencies
  | mcc == 7394 = "&#x1F3D7;" -- Equipment Rental
  | mcc == 7395 = "&#x1F4F8;" -- Photo Developing
  | mcc == 7399 = "&#x1F4B3;" -- Miscellaneous Business Services
  | mcc == 7511 = "&#x1F698;" -- Truck Stop
  | mcc == 7512 = "&#x1F698;" -- Car Rental Agencies
  | mcc == 7513 = "&#x1F698;" -- Truck/Utility Trailer Rentals
  | mcc == 7519 = "&#x1F698;" -- Recreational Vehicle Rentals
  | mcc == 7523 = "&#x1F698;" -- Parking Lots, Garages
  | mcc == 7531 = "&#x1F698;" -- Auto Body Repair Shops
  | mcc == 7534 = "&#x1F698;" -- Tire Retreading and Repair
  | mcc == 7535 = "&#x1F698;" -- Auto Paint Shops
  | mcc == 7538 = "&#x1F698;" -- Auto Service Shops
  | mcc == 7542 = "&#x1F698;" -- Car Washes
  | mcc == 7549 = "&#x1F698;" -- Towing Services
  | mcc == 7622 = "&#x1F50C;" -- Electronics Repair Shops
  | mcc == 7623 = "&#x1F50C;" -- A/C, Refrigeration Repair
  | mcc == 7629 = "&#x1F50C;" -- Small Appliance Repair
  | mcc == 7631 = "&#x231A;&#xFE0F;" -- Watch/Jewelry Repair
  | mcc == 7641 = "&#x1FA91;" -- Furniture Repair, Refinishing
  | mcc == 7692 = "&#x1F469;&#x200D;&#x1F3ED;" -- Welding Repair
  | mcc == 7699 = "&#x1F469;&#x200D;&#x1F3ED;" -- Miscellaneous Repair Shops
  | mcc == 7829 = "&#x1F4F8;" -- Picture/Video Production
  | mcc == 7832 = "&#x1F37F;" -- Motion Picture Theaters
  | mcc == 7841 = "&#x1F37F;" -- Video Tape Rental Stores
  | mcc == 7911 = "&#x1F57A;" -- Dance Hall, Studios, Schools
  | mcc == 7922 = "&#x1F3AD;" -- Theatrical Ticket Agencies
  | mcc == 7929 = "&#x1F3B7;" -- Bands, Orchestras
  | mcc == 7932 = "&#x1F3B1;" -- Billiard/Pool Establishments
  | mcc == 7933 = "&#x1F3B3;" -- Bowling Alleys
  | mcc == 7941 = "&#x1F3D1;" -- Sports Clubs/Fields
  | mcc == 7991 = "&#x1F5FD;" -- Tourist Attractions and Exhibits
  | mcc == 7992 = "&#x26F3;&#xFE0F;" -- Golf Courses - Public
  | mcc == 7993 = "&#x1F47E;" -- Video Amusement Game Supplies
  | mcc == 7994 = "&#x1F47E;" -- Video Game Arcades
  | mcc == 7995 = "&#x1F3B0;" -- Betting/Casino Gambling
  | mcc == 7996 = "&#x1F3A2;" -- Amusement Parks/Carnivals
  | mcc == 7997 = "&#x26F3;&#xFE0F;" -- Country Clubs
  | mcc == 7998 = "&#x1F420;" -- Aquariums
  | mcc == 7999 = "&#x1F3D3;" -- Miscellaneous Recreation Services
  | mcc == 8011 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Doctors
  | mcc == 8021 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Dentists, Orthodontists
  | mcc == 8031 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Osteopaths
  | mcc == 8041 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Chiropractors
  | mcc == 8042 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Optometrists, Ophthalmologist
  | mcc == 8043 = "&#x1F453;" -- Opticians, Eyeglasses
  | mcc == 8049 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Chiropodists, Podiatrists
  | mcc == 8050 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Nursing/Personal Care
  | mcc == 8062 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Hospitals
  | mcc == 8071 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Medical and Dental Labs
  | mcc == 8099 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Medical Services
  | mcc == 8111 = "&#x1F469;&#x200D;&#x2696;&#xFE0F;" -- Legal Services, Attorneys
  | mcc == 8211 = "&#x1F393;" -- Elementary, Secondary Schools
  | mcc == 8220 = "&#x1F393;" -- Colleges, Universities
  | mcc == 8241 = "&#x1F393;" -- Correspondence Schools
  | mcc == 8244 = "&#x1F393;" -- Business/Secretarial Schools
  | mcc == 8249 = "&#x1F393;" -- Vocational/Trade Schools
  | mcc == 8299 = "&#x1F393;" -- Educational Services
  | mcc == 8351 = "&#x1F423;" -- Child Care Services
  | mcc == 8398 = "&#x1F397;" -- Charitable and Social Service Organizations - Fundraising
  | mcc == 8641 = "&#x1F397;" -- Civic, Social, Fraternal Associations
  | mcc == 8651 = "&#x1F4B3;" -- Political Organizations
  | mcc == 8661 = "&#x1F4B3;" -- Religious Organizations
  | mcc == 8675 = "&#x1F4B3;" -- Automobile Associations
  | mcc == 8699 = "&#x1F4B3;" -- Membership Organizations
  | mcc == 8734 = "&#x1F469;&#x200D;&#x2695;&#xFE0F;" -- Testing Laboratories
  | mcc == 8911 = "&#x1F469;&#x200D;&#x1F4BC;" -- Architectural/Surveying Services
  | mcc == 8931 = "&#x1F469;&#x200D;&#x1F4BC;" -- Accounting/Bookkeeping Services
  | mcc == 8999 = "&#x1F469;&#x200D;&#x1F4BC;" -- Professional Services
  | mcc == 9211 = "&#x1F469;&#x200D;&#x2696;&#xFE0F;" -- Court Costs, Including Alimony and Child Support - Courts of Law
  | mcc == 9222 = "&#x1F469;&#x200D;&#x2696;&#xFE0F;" -- Fines - Government Administrative Entities
  | mcc == 9223 = "&#x1F469;&#x200D;&#x2696;&#xFE0F;" -- Bail and Bond Payments (payment to the surety for the bond, not the actual bond paid to the government agency)
  | mcc == 9311 = "&#x1F4B8;" -- Tax Payments - Government Agencies
  | mcc == 9399 = "&#x1F3DB;" -- Government Services (Not Elsewhere Classified)
  | mcc == 9402 = "&#x1F4EC;" -- Postal Services - Government Only
  | mcc == 9405 = "&#x1F3DB;" -- U.S. Federal Government Agencies or Departments
  | mcc == 9950 = "&#x1F4B3;" -- Intra-Company Purchases
  | otherwise = "&#x1F4B3;"
