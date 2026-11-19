// The production icon, as GitHub shows the Hanzo Dev app, whatever this build's channel.
import appIconUrl from "../../../../assets/prod/hanzo-black-web-apple-touch-180.png";

export function HanzoAppIcon(props: { readonly className?: string }) {
  return <img src={appIconUrl} alt="" className={props.className} />;
}
