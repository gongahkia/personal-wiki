"use client"

import { motion } from "framer-motion"
import { CyclingPolygon } from "@/components/cycling-polygon"
import Link from "next/link"

export default function Contact() {
  return (
    <main className="min-h-screen flex items-center justify-center bg-white px-2 py-4">
      <div className="flex items-center justify-between w-full max-w-sm">
        <Link href="/">
          <CyclingPolygon className="w-12 h-12 text-gray-800" />
        </Link>

        <motion.div
          className="mx-2 space-y-6 text-center"
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.8 }}
        >
          <p className="text-gray-800 font-serif text-sm">If I miss your e-mail, please contact me on LinkedIn.</p>

          <div className="space-y-4">
            <div>
              <h2 className="text-lg font-serif mb-2">E-mail</h2>
              <p className="space-y-1 text-sm">
                <span className="block">
                  Business:{" "}
                  <Link href="mailto:gabrielzmong@gmail.com" className="text-gray-600 hover:text-black italic">
                    gabrielzmong@gmail.com
                  </Link>
                </span>
                <span className="block">
                  School:{" "}
                  <Link href="mailto:gabriel.ong.2023@scis.smu.edu.sg" className="text-gray-600 hover:text-black italic">
                    gabriel.ong.2023@scis.smu.edu.sg
                  </Link>
                </span>
              </p>
            </div>

            <div>
              <h2 className="text-lg font-serif mb-2">LinkedIn</h2>
              <Link
                href="https://www.linkedin.com/in/gabriel-zmong"
                className="text-gray-600 hover:text-black italic text-sm"
                target="_blank"
                rel="noopener noreferrer"
              >
                https://www.linkedin.com/in/gabriel-zmong
              </Link>
            </div>
          </div>

          <footer className="text-center text-gray-600 font-serif text-xs">
            <p>© {new Date().getFullYear()} Gabriel Ong / All rights reserved.</p>
          </footer>
        </motion.div>

        <div>
          <CyclingPolygon className="w-12 h-12 text-gray-800" />
        </div>
      </div>
    </main>
  )
}