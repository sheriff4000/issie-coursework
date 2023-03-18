import { interpolate, toConsole } from "../fable_modules/fable-library.4.0.0-theta-018/String.js";

export function smartChannelRoute(channelOrientation, channel, model) {
    const tl = channel.TopLeft;
    toConsole(interpolate("SmartChannel: channel %P():(%.1f%P(),%.1f%P()) W=%.1f%P() H=%.1f%P()", [channelOrientation, tl.X, tl.Y, channel.W, channel.H]));
    return model;
}

//# sourceMappingURL=SmartChannel.fs.js.map
