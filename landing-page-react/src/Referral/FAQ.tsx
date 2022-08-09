const faqs = [
  {
    question: "What is the 5/$20/$20 Referral Program?",
    answer:
      "Invite friends, family and acquaintances to join Pay Tgthr, and you’ll receive a $20 credit to your Pay Tgthr account for your first 5 successful referrals and a $40 credit for all subsequent successful referrals!",
  },
  {
    question: "What is a “successful referral”?",
    answer:
      "To be considered a “successful referral”, the person you refer must: (1) be approved for a new Pay Tgthr account, (2) invite a partner and form a group, and (3) the group must make 5 purchases of at least $20 each within 60 days of entering your referral code.",
  },
  {
    question: "Who can make a referral?",
    answer:
      "Any Pay Tgthr user who has an active group in which both partners have linked and verified bank accounts, do not have an overdue balance, and have made at least $150 in purchases.",
  },
  {
    question: "How do I make a referral using my unique referral code?",
    answer: (
      <>
        <p className="text-base text-gray-500 mb-2">
          Open the Pay Tgthr app and navigate to the Settings menu. Tap “Share
          your referral code” to get your unique referral code on the next
          screen. Share your code with friends, family and acquaintances. You
          can send the link via a text message or email. By sending your
          referral code, you confirm you have the receiver’s consent to contact
          them
        </p>
        <p className="text-base text-gray-500">
          Pay Tgthr reserves the right to refuse to issue a referral reward if
          it determines, in its sole discretion, that the reward was claimed
          under wrongful or fraudulent circumstances. If we suspect that someone
          is abusing the Program, for example, by posting their unique referral
          code publicly on social media, we may terminate their eligibility to
          receive referral bonuses.
        </p>
      </>
    ),
  },
  {
    question: "How many people can I refer?",
    answer: "As many as you'd like!",
  },
  {
    question: "Can I refer my partner?",
    answer:
      "No. You cannot refer the person with whom you intend to or do form a group. The referred person can be any other friend, family member or acquaintance that will be able to fulfill the criteria for a “successful referral”.",
  },
  {
    question:
      "Can I refer a person who has already started signing up for Pay Tgthr?",
    answer:
      "No. The person you refer cannot have registered or begun registering a Pay Tgthr account in the past. If any personal information, including name, email address, phone number, social security number or bank account information has previously been used to register or begin registration of a Pay Tgthr account, that person will not count as a successful referral.",
  },
  {
    question: "What reward will I receive?",
    answer:
      "You’ll receive a $20 credit to your Pay Tgthr account for each of your first 5 successful referrals, and a $40 credit for each successful referral after that!",
  },
  {
    question: "Will the person I refer receive a reward?",
    answer:
      "Yes! Once the person you refer fulfills the criteria for a successful referral, they will receive a $20 credit to their Pay Tgthr account.",
  },
  {
    question: "How can credits be used?",
    answer:
      "Credits will be applied to your Pay Tgthr account and will first be applied to any outstanding balance you owe. Any remaining credits can be used toward future purchases on your Tgthr Card. For clarity, credits may not be withdrawn for cash value.",
  },
  {
    question:
      "If my referral registers for a Pay Tgthr account another way, will I receive the referral reward?",
    answer:
      "No. The person you refer must enter your unique referral code within 48 hours of registering for a Pay Tgthr account. If they do not enter your unique referral code or attempt to do so more than 48 hours after starting the registration process, you will be ineligible to receive a referral reward for that person.",
  },
  {
    question: "When will referral rewards be issued?",
    answer:
      "We aim to have referral rewards issued within 48 hours of achieving the criteria for a successful referral, though the credit may take up to 7 days to appear in your Pay Tgthr account.",
  },
  {
    question:
      "I was referred. Will the person who referred me know that I’ve opened a Pay Tgthr account and that I’ve made 5 qualifying purchases?",
    answer:
      "By opening a Pay Tgthr account using a unique referral code, you acknowledge and agree that your referer may be notified if you successfully open a Pay Tgthr account and complete 5 qualifying purchases. We will not disclose the details about those purchases (e.g., merchant name, location, amount, etc.). If you do not want your referer to have this limited information, please use this link to sign up for a Pay Tgthr account without using the referrer’s unique referral code.",
  },
  {
    question: "Will Pay Tgthr make changes to the Referral Program?",
    answer:
      "Pay Tgthr reserves the right to change the Referral Program terms and requirements, or terminate the Referral Program, at any time, in its sole discretion, and without notice. If you do not accept the changes made to the Referral Program, please do not participate or continue your participation in the Referral Program.",
  },
];

export default function ReferralFAQ() {
  return (
    <div className="bg-gray-50">
      <div className="max-w-7xl mx-auto py-12 px-4 divide-y divide-gray-200 sm:px-6 lg:py-16 lg:px-8">
        <h2 className="text-3xl font-extrabold text-gray-900">
          Frequently asked questions
        </h2>
        <div className="mt-8">
          <dl className="divide-y divide-gray-200">
            {faqs.map((faq, index) => (
              <div
                key={`${index}-${faq.question}`}
                className="pt-6 pb-8 md:grid md:grid-cols-12 md:gap-8"
              >
                <dt className="text-base font-medium text-gray-900 md:col-span-5">
                  {faq.question}
                </dt>
                <dd className="mt-2 md:mt-0 md:col-span-7">
                  <p className="text-base text-gray-500">{faq.answer}</p>
                </dd>
              </div>
            ))}
          </dl>
        </div>
      </div>
    </div>
  );
}
