import Image from "next/image";
import Link from "next/link";

export default function Home() {
  return (
    <div className="font-sans grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20">
      <main className="flex flex-col gap-[32px] row-start-2 items-center sm:items-start">
        <div className="flex items-center gap-4">
          <Image
            className="dark:invert"
            src="/next.svg"
            alt="Next.js logo"
            width={180}
            height={38}
            priority
          />
          <div className="w-2 h-8 bg-primary"></div>
          <h1 className="text-2xl font-bold text-foreground">Next.js Archetype</h1>
        </div>
        
        <p className="text-center sm:text-left text-muted-foreground max-w-md">
          Welcome to the Next.js application archetype. A modern frontend template ready for your features.
        </p>

        <div className="grid grid-cols-1 sm:grid-cols-2 gap-4 w-full max-w-md">
          <Link 
            href="/pages/users"
            className="rounded-lg border border-border bg-card hover:bg-accent hover:border-primary transition-colors px-5 py-4 cursor-pointer"
          >
            <h2 className="text-lg font-semibold text-foreground mb-2">
              User Management
            </h2>
            <p className="text-sm text-muted-foreground">
              Manage users, view list, add, edit, and delete users.
            </p>
          </Link>
          
          <div className="rounded-lg border border-border bg-muted px-5 py-4 opacity-60">
            <h2 className="text-lg font-semibold text-muted-foreground mb-2">
              Ready to Build
            </h2>
            <p className="text-sm text-muted-foreground">
              Add types, services, and pages for your domain.
            </p>
          </div>
        </div>

        <div className="flex gap-4 items-center flex-col sm:flex-row">
          <Link
            className="rounded-full border border-solid border-transparent transition-colors flex items-center justify-center bg-primary text-primary-foreground gap-2 hover:bg-primary/90 font-medium text-sm sm:text-base h-10 sm:h-12 px-4 sm:px-5 sm:w-auto"
            href="/login"
          >
            Sign In
          </Link>
          <Link
            className="rounded-full border border-solid border-border transition-colors flex items-center justify-center hover:bg-muted font-medium text-sm sm:text-base h-10 sm:h-12 px-4 sm:px-5 w-full sm:w-auto"
            href="/pages/users"
          >
            User Management
          </Link>
          <a
            className="rounded-full border border-solid border-border transition-colors flex items-center justify-center hover:bg-muted font-medium text-sm sm:text-base h-10 sm:h-12 px-4 sm:px-5 w-full sm:w-auto md:w-[158px]"
            href="https://nextjs.org/docs?utm_source=create-next-app&utm_medium=appdir-template-tw&utm_campaign=create-next-app"
            target="_blank"
            rel="noopener noreferrer"
          >
            Documentation
          </a>
        </div>
      </main>
      <footer className="row-start-3 flex gap-[24px] flex-wrap items-center justify-center">
        <p className="text-sm text-muted-foreground">
          Next.js Archetype © 2025
        </p>
      </footer>
    </div>
  );
}
