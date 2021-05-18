#![cfg_attr(not(feature = "std"), no_std)]

use crate::RandomnessCollectiveFlip;
use codec::{Decode, Encode};
use frame_support::{
    dispatch::DispatchResult,
    ensure,
    traits::{Currency, ExistenceRequirement, OriginTrait},
};
use frame_system::ensure_signed;
pub use pallet::*;
use sp_runtime::traits::{Hash, IdentityLookup, Zero};
use sp_std::cmp;
use sp_std::prelude::*;

#[derive(Encode, Decode, Default, Clone, PartialEq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct Kitty<Hash, Balance> {
    id: Hash,
    dna: Hash,
    price: Balance,
    gen: u64,
}

#[frame_support::pallet]
pub mod pallet {
    use super::*;
    use frame_support::pallet_prelude::*;
    use frame_system::pallet_prelude::*;

    #[pallet::pallet]
    #[pallet::generate_store(trait Store)]
    pub struct Pallet<T>(_);

    #[pallet::hooks]
    impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {}

    #[pallet::config]
    pub trait Config: pallet_balances::Config + frame_system::Config {
        type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
    }

    #[pallet::genesis_config]
    pub struct GenesisConfig<T: Config> {
        pub kitties: Vec<T::AccountId>,
    }

    // Required to implement default for GenesisConfig
    #[cfg(feature = "std")]
    impl<T: Config> Default for GenesisConfig<T> {
        fn default() -> GenesisConfig<T> {
            GenesisConfig { kitties: vec![] }
        }
    }

    #[pallet::genesis_build]
    impl<T: Config> GenesisBuild<T> for GenesisConfig<T> {
        fn build(&self) {
            &self
                .kitties
                .iter()
                .map(|kitty| <Module<T>>::create_kitty(T::Origin::signed(kitty)));
        }
    }

    #[pallet::call]
    impl<T: Config> Pallet<T> {
        #[pallet::weight(100)]
        pub fn create_kitty(origin: OriginFor<T>) -> DispatchResultWithPostInfo {
            let sender = ensure_signed(origin)?;
            let nonce = <Nonce<T>>::get();
            let seed = RandomnessCollectiveFlip::random_material();
            let random_hash = T::Hashing::hash_of(&(seed, &sender, nonce));

            let new_kitty = Kitty {
                id: random_hash,
                dna: random_hash,
                price: 0u8.into(),
                gen: 0,
            };

            Self::mint(sender, random_hash, new_kitty)?;
            <Nonce<T>>::mutate(|n| *n += 1);

            Ok(().into())
        }

        #[pallet::weight(100)]
        pub fn set_price(
            origin: OriginFor<T>,
            kitty_id: T::Hash,
            new_price: T::Balance,
        ) -> DispatchResultWithPostInfo {
            let sender = ensure_signed(origin)?;

            ensure!(
                <Kitties<T>>::contains_key(kitty_id),
                "This cat does not exist"
            );

            let owner = Self::owner_of(kitty_id).ok_or("No owner for this kitty")?;
            ensure!(owner == sender, "You do not own this cat");

            let mut kitty = Self::kitty(kitty_id);
            kitty.price = new_price;

            <Kitties<T>>::insert(kitty_id, kitty);

            Self::deposit_event(Event::PriceSet(sender, kitty_id, new_price));

            Ok(().into())
        }

        #[pallet::weight(100)]
        pub fn transfer(
            origin: OriginFor<T>,
            to: T::AccountId,
            kitty_id: T::Hash,
        ) -> DispatchResultWithPostInfo {
            let sender = ensure_signed(origin)?;

            let owner = Self::owner_of(kitty_id).ok_or("No owner for this kitty")?;
            ensure!(owner == sender, "You do not own this kitty");

            Self::transfer_from(sender, to, kitty_id)?;

            Ok(().into())
        }

        #[pallet::weight(100)]
        pub fn buy_kitty(
            origin: OriginFor<T>,
            kitty_id: T::Hash,
            max_price: T::Balance,
        ) -> DispatchResultWithPostInfo {
            let sender = ensure_signed(origin)?;

            ensure!(
                <Kitties<T>>::contains_key(kitty_id),
                "This cat does not exist"
            );

            let owner = Self::owner_of(kitty_id).ok_or("No owner for this kitty")?;
            ensure!(owner != sender, "You can't buy your own cat");

            let mut kitty = Self::kitty(kitty_id);

            let kitty_price = kitty.price;
            ensure!(
                !kitty_price.is_zero(),
                "The cat you want to buy is not for sale"
            );
            ensure!(
                kitty_price <= max_price,
                "The cat you want to buy costs more than your max price"
            );

            <pallet_balances::Module<T> as Currency<_>>::transfer(
                &sender,
                &owner,
                kitty_price,
                ExistenceRequirement::KeepAlive,
            )?;

            Self::transfer_from(owner.clone(), sender.clone(), kitty_id).expect(
                "`owner` is shown to own the kitty; \
                `owner` must have greater than 0 kitties, so transfer cannot cause underflow; \
                `all_kitty_count` shares the same type as `owned_kitty_count` \
                and minting ensure there won't ever be more than `max()` kitties, \
                which means transfer cannot cause an overflow; \
                qed",
            );

            kitty.price = 0u8.into();
            <Kitties<T>>::insert(kitty_id, kitty);

            Self::deposit_event(Event::Bought(sender, owner, kitty_id, kitty_price));

            Ok(().into())
        }

        #[pallet::weight(100)]
        pub fn breed_kitty(
            origin: OriginFor<T>,
            kitty_id_1: T::Hash,
            kitty_id_2: T::Hash,
        ) -> DispatchResultWithPostInfo {
            let sender = ensure_signed(origin)?;

            ensure!(
                <Kitties<T>>::contains_key(kitty_id_1),
                "This cat 1 does not exist"
            );
            ensure!(
                <Kitties<T>>::contains_key(kitty_id_2),
                "This cat 2 does not exist"
            );

            let nonce = <Nonce<T>>::get();
            let seed = RandomnessCollectiveFlip::random_material();
            let random_hash = T::Hashing::hash_of(&(seed, &sender, nonce));

            let kitty_1 = Self::kitty(kitty_id_1);
            let kitty_2 = Self::kitty(kitty_id_2);

            let mut final_dna = kitty_1.dna;
            for (i, (dna_2_element, r)) in kitty_2
                .dna
                .as_ref()
                .iter()
                .zip(random_hash.as_ref().iter())
                .enumerate()
            {
                if r % 2 == 0 {
                    final_dna.as_mut()[i] = *dna_2_element;
                }
            }

            let new_kitty = Kitty {
                id: random_hash,
                dna: final_dna,
                price: 0u8.into(),
                gen: cmp::max(kitty_1.gen, kitty_2.gen) + 1,
            };

            Self::mint(sender, random_hash, new_kitty)?;

            <Nonce<T>>::mutate(|n| *n += 1);

            Ok(().into())
        }
    }

    impl<T: Config> Pallet<T> {
        fn mint(
            to: T::AccountId,
            kitty_id: T::Hash,
            new_kitty: Kitty<T::Hash, T::Balance>,
        ) -> DispatchResult {
            ensure!(
                !<KittyOwner<T>>::contains_key(kitty_id),
                "Kitty already contains_key"
            );

            let owned_kitty_count = Self::owned_kitty_count(&to);

            let new_owned_kitty_count = owned_kitty_count
                .checked_add(1)
                .ok_or("Overflow adding a new kitty to account balance")?;

            let all_kitties_count = Self::all_kitties_count();

            let new_all_kitties_count = all_kitties_count
                .checked_add(1)
                .ok_or("Overflow adding a new kitty to total supply")?;

            <Kitties<T>>::insert(kitty_id, new_kitty);
            <KittyOwner<T>>::insert(kitty_id, Some(&to));

            <AllKittiesArray<T>>::insert(all_kitties_count, kitty_id);
            <AllKittiesCount<T>>::put(new_all_kitties_count);
            <AllKittiesIndex<T>>::insert(kitty_id, all_kitties_count);

            <OwnedKittiesArray<T>>::insert((to.clone(), owned_kitty_count), kitty_id);
            <OwnedKittiesCount<T>>::insert(&to, new_owned_kitty_count);
            <OwnedKittiesIndex<T>>::insert(kitty_id, owned_kitty_count);

            Self::deposit_event(Event::Created(to, kitty_id));

            Ok(())
        }

        fn transfer_from(
            from: T::AccountId,
            to: T::AccountId,
            kitty_id: T::Hash,
        ) -> DispatchResult {
            let owner = Self::owner_of(kitty_id).ok_or("No owner for this kitty")?;

            ensure!(owner == from, "'from' account does not own this kitty");

            let owned_kitty_count_from = Self::owned_kitty_count(&from);
            let owned_kitty_count_to = Self::owned_kitty_count(&to);

            let new_owned_kitty_count_to = owned_kitty_count_to
                .checked_add(1)
                .ok_or("Transfer causes overflow of 'to' kitty balance")?;

            let new_owned_kitty_count_from = owned_kitty_count_from
                .checked_sub(1)
                .ok_or("Transfer causes underflow of 'from' kitty balance")?;

            let kitty_index = <OwnedKittiesIndex<T>>::get(kitty_id);
            if kitty_index != new_owned_kitty_count_from {
                let last_kitty_id =
                    <OwnedKittiesArray<T>>::get((from.clone(), new_owned_kitty_count_from));
                <OwnedKittiesArray<T>>::insert((from.clone(), kitty_index), last_kitty_id);
                <OwnedKittiesIndex<T>>::insert(last_kitty_id, kitty_index);
            }

            <KittyOwner<T>>::insert(&kitty_id, Some(&to));
            <OwnedKittiesIndex<T>>::insert(kitty_id, owned_kitty_count_to);

            <OwnedKittiesArray<T>>::remove((from.clone(), new_owned_kitty_count_from));
            <OwnedKittiesArray<T>>::insert((to.clone(), owned_kitty_count_to), kitty_id);

            <OwnedKittiesCount<T>>::insert(&from, new_owned_kitty_count_from);
            <OwnedKittiesCount<T>>::insert(&to, new_owned_kitty_count_to);

            Self::deposit_event(Event::Transferred(from, to, kitty_id));

            Ok(())
        }
    }

    // EVENTS
    #[pallet::event]
    #[pallet::metadata(T::AccountId = "AccountId")]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    pub enum Event<T: Config> {
        Created(T::AccountId, T::Hash),
        PriceSet(T::AccountId, T::Hash, T::Balance),
        Transferred(T::AccountId, T::AccountId, T::Hash),
        Bought(T::AccountId, T::AccountId, T::Hash, T::Balance),
    }

    // STORAGE IMPLEMENTATIONS
    #[pallet::storage]
    #[pallet::getter(fn get_nonce)]
    pub(super) type Nonce<T: Config> = StorageValue<_, u64, ValueQuery>;

    #[pallet::storage]
    #[pallet::getter(fn kitty)]
    pub(super) type Kitties<T: Config> =
        StorageMap<_, Twox64Concat, T::Hash, Kitty<T::Hash, T::Balance>, ValueQuery>;

    #[pallet::storage]
    #[pallet::getter(fn owner_of)]
    pub(super) type KittyOwner<T: Config> =
        StorageMap<_, Twox64Concat, T::Hash, Option<T::AccountId>, ValueQuery>;

    #[pallet::storage]
    #[pallet::getter(fn kitty_by_index)]
    pub(super) type AllKittiesArray<T: Config> =
        StorageMap<_, Twox64Concat, u64, T::Hash, ValueQuery>;

    #[pallet::storage]
    #[pallet::getter(fn all_kitties_count)]
    pub(super) type AllKittiesCount<T: Config> = StorageValue<_, u64, ValueQuery>;

    #[pallet::storage]
    pub(super) type AllKittiesIndex<T: Config> =
        StorageMap<_, Twox64Concat, T::Hash, u64, ValueQuery>;

    #[pallet::storage]
    #[pallet::getter(fn kitty_of_owner_by_index)]
    pub(super) type OwnedKittiesArray<T: Config> =
        StorageMap<_, Twox64Concat, (T::AccountId, u64), T::Hash, ValueQuery>;

    #[pallet::storage]
    #[pallet::getter(fn owned_kitty_count)]
    pub(super) type OwnedKittiesCount<T: Config> =
        StorageMap<_, Twox64Concat, T::AccountId, u64, ValueQuery>;

    #[pallet::storage]
    pub(super) type OwnedKittiesIndex<T: Config> =
        StorageMap<_, Twox64Concat, T::Hash, u64, ValueQuery>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::substratekitties;
    use frame_support::{assert_noop, assert_ok, parameter_types};
    use sp_core::H256;
    use sp_runtime::{
        testing::Header,
        traits::{BlakeTwo256, IdentityLookup},
        BuildStorage,
    };

    type UncheckedExtrinsic = frame_system::mocking::MockUncheckedExtrinsic<Test>;
    type Block = frame_system::mocking::MockBlock<Test>;

    // Configure a mock runtime to test the pallet.
    frame_support::construct_runtime!(
        pub enum Test where
            Block = Block,
            NodeBlock = Block,
            UncheckedExtrinsic = UncheckedExtrinsic,
        {
            System: frame_system::{Module, Call, Config, Storage, Event<T>},
            Balances: pallet_balances::{Module, Call, Storage, Config<T>, Event<T>},
            Substratekitties: substratekitties::{Module, Call, Storage, Config<T>, Event<T>},
        }
    );

    parameter_types! {
        pub const BlockHashCount: u64 = 250;
        pub const SS58Prefix: u8 = 42;
    }

    impl frame_system::Config for Test {
        type AccountData = pallet_balances::AccountData<u64>;
        type AccountId = u64;
        type BaseCallFilter = ();
        type BlockHashCount = BlockHashCount;
        type BlockLength = ();
        type BlockNumber = u64;
        type BlockWeights = ();
        type Call = Call;
        type DbWeight = ();
        type Event = Event;
        type Hash = H256;
        type Hashing = BlakeTwo256;
        type Header = Header;
        type Index = u64;
        type Lookup = IdentityLookup<Self::AccountId>;
        type OnKilledAccount = ();
        type OnNewAccount = ();
        type Origin = Origin;
        type PalletInfo = PalletInfo;
        type SS58Prefix = ();
        type SystemWeightInfo = ();
        type Version = ();
    }

    parameter_types! {
        pub const ExistentialDeposit: u64 = 1;
    }

    impl pallet_balances::Config for Test {
        type MaxLocks = ();
        type Balance = u64;
        type DustRemoval = ();
        type Event = Event;
        type ExistentialDeposit = ExistentialDeposit;
        type AccountStore = System;
        type WeightInfo = ();
    }

    impl super::Config for Test {
        type Event = Event;
    }

    type Kitties = super::Module<Test>;

    pub(crate) fn new_test_ext() -> sp_io::TestExternalities {
        let mut t = frame_system::GenesisConfig::default()
            .build_storage::<Test>()
            .unwrap();
        crate::GenesisConfig {
            substratekitties: Some(crate::SubstratekittiesConfig { kitties: vec![10] }),
            ..Default::default()
        }
        .assimilate_storage(&mut t)
        .unwrap();
        t.into()
    }

    #[test]
    fn create_kitty_should_work() {
        new_test_ext().execute_with(|| {
            // create a kitty with account #10.
            assert_ok!(Kitties::create_kitty(Origin::signed(10)));

            // check that there is now 1 kitty in storage
            assert_eq!(Kitties::all_kitties_count(), 1);

            // check that account #10 owns 1 kitty
            assert_eq!(Kitties::owned_kitty_count(10), 1);

            // check that some random account #5 does not own a kitty
            assert_eq!(Kitties::owned_kitty_count(5), 0);

            // check that this kitty is specifically owned by account #10
            let hash = Kitties::kitty_by_index(0);
            assert_eq!(Kitties::owner_of(hash), Some(10));

            let other_hash = Kitties::kitty_of_owner_by_index((10, 0));
            assert_eq!(hash, other_hash);
        });
    }

    #[test]
    fn transfer_kitty_should_work() {
        new_test_ext().execute_with(|| {
            // check that 10 own a kitty
            assert_ok!(Kitties::create_kitty(Origin::signed(10)));

            assert_eq!(Kitties::owned_kitty_count(10), 1);
            let hash = Kitties::kitty_of_owner_by_index((10, 0));

            // send kitty to 1.
            assert_ok!(Kitties::transfer(Origin::signed(10), 1, hash));

            // 10 now has nothing
            assert_eq!(Kitties::owned_kitty_count(10), 0);
            // but 1 does
            assert_eq!(Kitties::owned_kitty_count(1), 1);
            let new_hash = Kitties::kitty_of_owner_by_index((1, 0));
            // and it has the same hash
            assert_eq!(hash, new_hash);
        });
    }

    #[test]
    fn transfer_not_owned_kitty_should_fail() {
        new_test_ext().execute_with(|| {
            // check that 10 own a kitty
            assert_ok!(Kitties::create_kitty(Origin::signed(10)));
            let hash = Kitties::kitty_of_owner_by_index((10, 0));

            // account 0 cannot transfer a kitty with this hash.
            assert_noop!(
                Kitties::transfer(Origin::signed(9), 1, hash),
                "You do not own this kitty"
            );
        });
    }

    #[test]
    fn should_build_genesis_kitties() {
        new_test_ext().execute_with(|| {
            // Check that 2nd kitty exists at genesis, with value 100
            let kitty0 = Kitties::kitty_by_index(0);
            let kitty1 = Kitties::kitty_by_index(1);

            // Check we have 2 kitties, as specified
            // assert_eq!(Kitties::all_kitties_count(), 2);

            // Check that they are owned correctly
            assert_eq!(Kitties::owner_of(kitty0), Some(0));
            assert_eq!(Kitties::owner_of(kitty1), Some(1));

            // Check owners own the correct amount of kitties
            // assert_eq!(Kitties::owned_kitty_count(0), 1);
            // assert_eq!(Kitties::owned_kitty_count(2), 0);
        });
    }
}
