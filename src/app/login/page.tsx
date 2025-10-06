'use client';

import { useRouter } from 'next/navigation';
import { useState } from 'react';
import LoginForm from '@/components/LoginForm';
import { UserLoginResponse } from '@/types/user';

export default function LoginPage() {
  const router = useRouter();
  const [isExiting, setIsExiting] = useState(false);

  const handleLogin = async (response: UserLoginResponse) => {
    // Store user session information
    if (typeof window !== 'undefined') {
      localStorage.setItem('userSession', JSON.stringify(response));
      localStorage.setItem('authToken', response.token);
    }

    // Redirect based on user type
    if (true) {
      // Admin user - redirect to admin dashboard
      router.push('/pages/users');
    } else {
      // Regular user - redirect to main dashboard
      router.push('/dashboard');
    }
  };

  const handleExit = () => {
    setIsExiting(true);
    
    // Clear any stored session data
    if (typeof window !== 'undefined') {
      localStorage.removeItem('userSession');
      localStorage.removeItem('authToken');
    }

    // In a real application, you might redirect to a goodbye page
    // or close the application window
    setTimeout(() => {
      window.close();
    }, 2000);
  };

  if (isExiting) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-center">
          <div className="mb-4">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          </div>
          <h1 className="text-2xl font-bold text-gray-900 mb-2">
            Thank you for using CardDemo Application
          </h1>
          <p className="text-gray-600">
            Goodbye! The application will close shortly.
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 flex items-center justify-center py-12 px-4 sm:px-6 lg:px-8">
      <div className="max-w-md w-full space-y-8">
        {/* Application Header */}
        <div className="text-center">
          <div className="mx-auto h-12 w-12 bg-blue-600 rounded-lg flex items-center justify-center mb-4">
            <svg
              className="h-8 w-8 text-white"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"
              />
            </svg>
          </div>
          <h2 className="text-3xl font-extrabold text-gray-900">
            CardDemo Application
          </h2>
          <p className="mt-2 text-sm text-gray-600">
            User Management System
          </p>
        </div>

        {/* Login Form */}
        <LoginForm
          onLogin={handleLogin}
          onExit={handleExit}
          className="bg-white shadow-xl"
        />

        {/* Footer */}
        <div className="text-center text-xs text-gray-500">
          <p>© 2024 CardDemo Application. All rights reserved.</p>
          <p className="mt-1">
            Modernized from COBOL CICS Application
          </p>
        </div>
      </div>
    </div>
  );
}