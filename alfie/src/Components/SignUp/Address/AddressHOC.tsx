import React, { useContext } from "react";
import { useSelector } from "react-redux";
import { useQuery } from "react-query";
import { ActivityIndicator, Alert, Platform, Text } from "react-native";
import {
  connectActionSheet,
  useActionSheet,
} from "@expo/react-native-action-sheet";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import { State } from "../../../State/State";
import FormScreen from "../../Form/FormScreen";
import {
  Address,
  getAddress,
  useSubmitAddress,
} from "../../../Actions/SignUp/SignUpAddress";
import { verifyAddress } from "../../../Actions/AddressVerification";
import TextStyles from "../../Styling/Text";
import UserStateRouter from "../../Routers/UserStateRouter";
import SignUpAddress from "./Address";
import { ThemeContext } from "../../Theming/ThemeContext";

interface Props {
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const SignupAddressHOC = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const { inProgress, submit, errors, submitted } = useSubmitAddress(baseURL);
  const query = useQuery(["/app/signup/address"], getAddress(baseURL));
  const { showActionSheetWithOptions } = useActionSheet();

  const verifyAndSubmit = async (entered: Address) => {
    try {
      const modified = await verifyAddress(entered);
      const showChooser = () =>
        showActionSheetWithOptions(
          {
            title: "Address correction",
            message: `We recomend using the following address:\n\n${formatedMessage}`,
            options: ["Use new address"],
          },
          async (index) => {
            switch (index) {
              case 0:
                await Analytics.track("Address Suggestion Accepted", {
                  entered: { ...entered },
                  modified: { ...modified },
                });
                return submit(modified);

              case 1:
                await Analytics.track("Address Suggestion Declined", {
                  entered: { ...entered },
                  modified: { ...modified },
                });
                return submit(entered);
            }
          }
        );

      const aptLine = modified.apt === "" ? "" : `${modified.apt}\n`;

      const formatedMessage = `${modified.street}\n${aptLine}${modified.city}, ${modified.state} ${modified.zip}`;

      if (
        modified.street.toLowerCase() !== entered.street.toLowerCase() ||
        modified.city.toLowerCase() !== entered.city.toLowerCase() ||
        modified.state.toLowerCase() !== entered.state.toLowerCase() ||
        modified.zip.toLowerCase() !== entered.zip.toLowerCase()
      ) {
        await Analytics.track("Address Change Suggested", {
          entered: { ...entered },
          modified: { ...modified },
        });
        showChooser();
      } else {
        submit(modified);
      }
    } catch (err) {
      await Analytics.track("Address Not Found", {
        ...entered,
      });

      if (Platform.OS === "android" || Platform.OS === "ios") {
        Alert.alert("Address couldn't be found", undefined, [{ text: "OK" }]);
      } else {
        showActionSheetWithOptions(
          {
            title: "Address couldn't be found",
            options: ["OK"],
          },
          () => true
        );
      }
    }

    //
  };

  const loadingScreen = (
    <FormScreen
      title="Residential address"
      navigation={{ type: "action", action: props.goBack }}
      buttons={<></>}
    >
      <ActivityIndicator size="large" color={theme.textColor} />
    </FormScreen>
  );

  if (submitted === true) {
    return <UserStateRouter loading={loadingScreen} push />;
  }

  switch (query.status) {
    case "loading":
    case "idle":
    default:
      return loadingScreen;

    case "error":
      return (
        <FormScreen
          title="Residential address"
          navigation={{ type: "action", action: props.goBack }}
          buttons={<></>}
        >
          <Text
            style={[
              theme.textStyle,
              TextStyles.fontWeight400,
              TextStyles.centered,
            ]}
          >
            Uh oh, houston we have a problem
          </Text>
        </FormScreen>
      );

    case "success":
      return (
        <SignUpAddress
          inProgress={inProgress}
          submit={verifyAndSubmit}
          errors={errors}
          trackEvent={Analytics.track}
          {...query.data}
          {...props}
        />
      );
  }
};

export default connectActionSheet(SignupAddressHOC);
