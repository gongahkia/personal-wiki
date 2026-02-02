import "./globals.css"
import { Playfair_Display } from "next/font/google"
import type React from "react"

const playfair = Playfair_Display({
  subsets: ["latin"],
  variable: "--font-playfair",
})

export const metadata = {
  title: "GABRIEL ONG",
  description: "Legal Technologist",
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className={`${playfair.variable} font-serif`}>{children}</body>
    </html>
  )
}