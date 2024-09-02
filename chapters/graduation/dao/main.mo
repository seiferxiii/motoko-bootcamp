import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import HashMap "mo:base/HashMap";
import Buffer "mo:base/Buffer";
import Option "mo:base/Option";
import Time "mo:base/Time";
import Nat64 "mo:base/Nat64";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Types "types";
actor {

        type Result<A, B> = Result.Result<A, B>;
        type Hash = Hash.Hash;
        type Member = Types.Member;
        type ProposalContent = Types.ProposalContent;
        type ProposalId = Types.ProposalId;
        type Proposal = Types.Proposal;
        type Vote = Types.Vote;
        type HttpRequest = Types.HttpRequest;
        type HttpResponse = Types.HttpResponse;

        // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
        stable let canisterIdWebpage : Principal = Principal.fromText("3jfrh-niaaa-aaaak-qiykq-cai");
        stable var manifesto = "Let's graduate!";
        stable let name = "GANAP DAO";
        var goals = Buffer.Buffer<Text>(0);

        let tokenCanister = actor ("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
                mint : shared (owner : Principal, amount : Nat) -> async Result<(), Text>;
                balanceOf : shared (member : Principal) -> async ?Nat;
                burn : shared (owner : Principal, amount : Nat) -> async Result<(), Text>;
        };

        let members : HashMap.HashMap<Principal, Member> = HashMap.HashMap<Principal, Member>(1, Principal.equal, Principal.hash);
        let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat.equal, Hash.hash);
        var nextProposalId : ProposalId = 0;

        let initialMember : Member = {
                name = "motoko_bootcamp";
                role = #Mentor;
        };
        let initialMentor = Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai");
        members.put(initialMentor, initialMember);

        // Returns the name of the DAO
        public query func getName() : async Text {
                return name;
        };

        // Returns the manifesto of the DAO
        public query func getManifesto() : async Text {
                return manifesto;
        };

        // Returns the goals of the DAO
        public query func getGoals() : async [Text] {
                return Buffer.toArray(goals);
        };

        // Register a new member in the DAO with the given name and principal of the caller
        // Airdrop 10 MBC tokens to the new member
        // New members are always Student
        // Returns an error if the member already exists
        public shared ({ caller }) func registerMember(name : Text) : async Result<(), Text> {
                switch (members.get(caller)) {
                        case (null) {
                                let newMember : Member = {
                                        name = name;
                                        role = #Student;
                                };
                                let mintToken = await tokenCanister.mint(caller, 10);
                                members.put(caller, newMember);
                                #ok();
                        };
                        case (?member) {
                                #err("Already registered");
                        };
                };
        };

        // Get the member with the given principal
        // Returns an error if the member does not exist
        public query func getMember(p : Principal) : async Result<Member, Text> {
                switch (members.get(p)) {
                        case (null) {
                                return #err("Member not found");
                        };
                        case (?member) {
                                return #ok(member);
                        };
                };
        };

        // Graduate the student with the given principal
        // Returns an error if the student does not exist or is not a student
        // Returns an error if the caller is not a mentor
        public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
                switch (members.get(caller)) {
                        case (null) {
                                return #err("Unauthorized access");
                        };
                        case (?mentor) {
                                if (mentor.role != #Mentor) {
                                        return #err("Not a mentor");
                                };
                                switch (members.get(student)) {
                                        case (null) {
                                                return #err("Member not found");
                                        };
                                        case (?member) {
                                                if (member.role != #Student) {
                                                        return #err("Permission denied");
                                                };
                                                let graduateMember : Member = {
                                                        name = member.name;
                                                        role = #Graduate;
                                                };
                                                members.put(student, graduateMember);
                                                #ok();
                                        };
                                };
                        };
                };
        };

        // Create a new proposal and returns its id
        // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
        public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
                switch (members.get(caller)) {
                        case (null) {
                                return #err("Unauthorized access");
                        };
                        case (?member) {
                                if (member.role != #Mentor) {
                                        return #err("You must be a mentor to create a proposal");
                                };
                                let balance = Option.get<Nat>(await tokenCanister.balanceOf(caller), 0);
                                if (balance < 1) {
                                        return #err("Insufficient token");
                                };
                                switch (content) {
                                        case (#AddMentor(newMentor)) {
                                                switch (members.get(newMentor)) {
                                                        case (null) {
                                                                return #err("Permission denied");
                                                        };
                                                        case (?tobeMentor) {
                                                                if (tobeMentor.role != #Graduate) {
                                                                        return #err("You need to graduate first before becoming a mentor");
                                                                };
                                                        };
                                                };
                                        };
                                        case (_) {};
                                };
                                let proposal : Proposal = {
                                        id = nextProposalId;
                                        content;
                                        creator = caller;
                                        created = Time.now();
                                        executed = null;
                                        votes = [];
                                        voteScore = 0;
                                        status = #Open;
                                };
                                proposals.put(nextProposalId, proposal);
                                nextProposalId += 1;
                                ignore tokenCanister.burn(caller, 1);
                                return #ok(nextProposalId - 1);
                        };
                };
        };

        // Get the proposal with the given id
        // Returns an error if the proposal does not exist
        public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
                switch (proposals.get(id)) {
                        case (null) {
                                #err("Proposal not found");
                        };
                        case (?proposal) {
                                return #ok(proposal);
                        };
                };
        };

        // Returns all the proposals
        public query func getAllProposal() : async [Proposal] {
                return Iter.toArray(proposals.vals());
        };

        // Vote for the given proposal
        // Returns an error if the proposal does not exist or the member is not allowed to vote
        public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
                switch (members.get(caller)) {
                        case (null) {
                                return #err("Permission denied");
                        };
                        case (?member) {
                                if (member.role == #Student) {
                                        return #err("Permission denied");
                                };
                                switch (proposals.get(proposalId)) {
                                        case (null) {
                                                return #err("Proposal not found");
                                        };
                                        case (?proposal) {
                                                if (proposal.status != #Open) {
                                                        return #err("The proposal is not yet open for voting");
                                                };
                                                if (_hasVoted(proposal, caller)) {
                                                        return #err("Invalid action");
                                                };
                                                let balance = Option.get<Nat>(await tokenCanister.balanceOf(caller), 0);
                                                let multiplierVote : Int = switch (yesOrNo) {
                                                        case (true) { 1 };
                                                        case (false) { -1 };
                                                };
                                                let newVotingPower : Nat = switch (member.role) {
                                                        case (#Graduate) {
                                                                balance;
                                                        };
                                                        case (#Mentor) {
                                                                balance * 5;
                                                        };
                                                        case (#Student) { 0 };
                                                };
                                                let newVoteScore = proposal.voteScore + (newVotingPower * multiplierVote);
                                                var newExecuted : ?Time.Time = null;
                                                let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                                                let vote : Vote = {
                                                        member = caller;
                                                        votingPower = newVotingPower;
                                                        yesOrNo = yesOrNo;
                                                };
                                                let currentVote = Buffer.fromArray<Vote>(proposal.votes);
                                                currentVote.add(vote);
                                                let newStatus = if (newVoteScore >= 100) {
                                                        #Accepted;
                                                } else if (newVoteScore <= -100) {
                                                        #Rejected;
                                                } else {
                                                        #Open;
                                                };
                                                switch (newStatus) {
                                                        case (#Accepted) {
                                                                _executeProposal(proposal.content);
                                                                newExecuted := ?Time.now();
                                                        };
                                                        case (_) {};
                                                };
                                                let newProposal : Proposal = {
                                                        id = proposal.id;
                                                        content = proposal.content;
                                                        creator = proposal.creator;
                                                        created = proposal.created;
                                                        executed = newExecuted;
                                                        votes = Buffer.toArray(currentVote);
                                                        voteScore = newVoteScore;
                                                        status = newStatus;
                                                };
                                                proposals.put(proposal.id, newProposal);
                                                return #ok();
                                        };
                                };
                        };
                };
        };

        func _executeProposal(content : ProposalContent) : () {
                switch (content) {
                        case (#ChangeManifesto(newManifesto)) {
                                manifesto := newManifesto;
                        };
                        case (#AddGoal(newGoal)) {
                                goals.add(newGoal);
                        };
                        case (#AddMentor(newMentor)) {
                                switch (members.get(newMentor)) {
                                        case (null) {};
                                        case (?mentor) {
                                                let mentorMember : Member = {
                                                        name = mentor.name;
                                                        role = #Mentor;
                                                };
                                                members.put(newMentor, mentorMember);
                                        };
                                };
                        };
                };
                return;
        };
        func _hasVoted(proposal : Proposal, member : Principal) : Bool {
                return Array.find<Vote>(
                        proposal.votes,
                        func(vote : Vote) {
                                return vote.member == member;
                        },
                ) != null;
        };
        // Returns the Principal ID of the Webpage canister associated with this DAO canister
        public query func getIdWebpage() : async Principal {
                return canisterIdWebpage;
        };

};
