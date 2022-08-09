import { StyleSheet } from "react-native";

const crispWhite = "#FFFFFF";
const nightBlack = "#000000";

const styles = StyleSheet.create({
  background: {
    bottom: 0,
    left: 0,
    position: "absolute",
    right: 0,
    top: 0,
  },
  bulletpoint: { marginLeft: 10 },
  bulletpointText: { color: nightBlack, fontSize: 13 },
  buttonSection: { marginBottom: -10 },
  footer: {
    color: nightBlack,
    fontSize: 13,
    marginTop: 0,
    textAlign: "center",
  },
  heading: { color: nightBlack, fontSize: 28, marginBottom: 10 },
  popup: {
    backgroundColor: crispWhite,
    borderRadius: 20,
    left: 0,
    marginHorizontal: 20,
    padding: 20,
    position: "absolute",
    right: 0,
  },
  subHeading: {
    color: nightBlack,
    fontSize: 16,
    marginBottom: 5,
  },
  textSection: { flex: 1, marginBottom: 30 },
});

export default styles;
