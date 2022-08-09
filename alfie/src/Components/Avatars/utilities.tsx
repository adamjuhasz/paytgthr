export const getNumber = (name:string):number => {
  const charactersArray = Array.from(name)
  let charactersCodesSum = 0

  charactersArray.forEach((charactersArrayItem) => {
    return charactersCodesSum += charactersArrayItem.charCodeAt(0)
  })

  return charactersCodesSum;
}

export const getModulus = (num: number, max:number): number => {
  return num % max;
}

export const getDigit = (number: number, ntn: number): number => {
  return Math.floor(( number/ Math.pow(10, ntn)) % 10);
}

export const getBoolean = (number: number, ntn: number): boolean => {
  return (!((getDigit(number, ntn)) % 2))
}

export const getAngle = (x: number, y: number): number => {
  return Math.atan2(y, x) * 180 / Math.PI;
}

export const getUnit = (number: number, range: number, index?: number): number => {
  const value = number % range

  if(index && ((getDigit(number, index) % 2) === 0)) {
    return -value
  } else return value
}

export const getRandomColor = (number: number, colors: string[], range: number): string => {
  return colors[(number) % range]
}


export const getContrast = (hexcolor: string): ('black' | 'white') => {

	// If a leading # is provided, remove it
	if (hexcolor.slice(0, 1) === '#') {
		hexcolor = hexcolor.slice(1);
	}

	// Convert to RGB value
	const r = parseInt(hexcolor.substr(0,2),16);
	const g = parseInt(hexcolor.substr(2,2),16);
	const b = parseInt(hexcolor.substr(4,2),16);

	// Get YIQ ratio
	const yiq = ((r * 299) + (g * 587) + (b * 114)) / 1000;

	// Check contrast
	return (yiq >= 128) ? 'black' : 'white';

};