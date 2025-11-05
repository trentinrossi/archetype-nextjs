'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import { userService } from '@/services/userService';
import UserForm from '@/components/UserForm';
import { CreateUserRequest } from '@/types/user';

export default function UserAddPage() {
  const router = useRouter();
  const [currentDateTime, setCurrentDateTime] = useState(new Date());
  const [currentUser, setCurrentUser] = useState<any>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);

  useEffect(() => {
    const user = userService.getCurrentUser();
    if (!user || user.userType !== 'A') {
      router.push('/login');
      return;
    }
    setCurrentUser(user);

    const timer = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);

    return () => clearInterval(timer);
  }, [router]);

  const formatDateTime = (date: Date): string => {
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const year = date.getFullYear();
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    const seconds = String(date.getSeconds()).padStart(2, '0');
    return `${month}/${day}/${year} ${hours}:${minutes}:${seconds}`;
  };

  const handleSubmit = async (data: CreateUserRequest) => {
    try {
      const response = await userService.createUser(data);
      setSuccessMessage(response.message);
      setTimeout(() => {
        router.push('/admin/users');
      }, 2000);
    } catch (err: any) {
      alert(err.message || 'Failed to create user');
    }
  };

  const handleCancel = () => {
    router.push('/admin/users');
  };

  const handlePF3 = () => {
    router.push('/admin');
  };

  if (!currentUser) {
    return null;
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 flex flex-col">
      <header className="bg-white shadow-md">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
          <div className="flex justify-between items-center">
            <div className="flex items-center">
              <div className="flex items-center justify-center w-10 h-10 bg-indigo-600 rounded-lg mr-3">
                <svg className="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M18 9v3m0 0v3m0-3h3m-3 0h-3m-2-5a4 4 0 11-8 0 4 4 0 018 0zM3 20a6 6 0 0112 0v1H3v-1z" />
                </svg>
              </div>
              <div>
                <h1 className="text-2xl font-bold text-gray-900">Add User</h1>
                <p className="text-xs text-gray-600">Transaction: CU01 | Program: COUSR01C</p>
              </div>
            </div>
            <div className="text-right">
              <div className="text-sm text-gray-600 font-medium">{formatDateTime(currentDateTime)}</div>
              <div className="text-xs text-gray-500 mt-1">User: {currentUser.firstName} {currentUser.lastName}</div>
            </div>
          </div>
        </div>
      </header>

      <main className="flex-1 px-4 sm:px-6 lg:px-8 py-8">
        <div className="max-w-2xl mx-auto">
          <div className="bg-white rounded-lg shadow-xl overflow-hidden">
            <div className="bg-gradient-to-r from-indigo-600 to-indigo-700 px-6 py-4">
              <h2 className="text-xl font-bold text-white">Create New User</h2>
              <p className="text-indigo-100 text-sm mt-1">Enter user information below</p>
            </div>

            <div className="p-6">
              {successMessage && (
                <div className="mb-6 bg-green-50 border border-green-200 rounded-lg p-4">
                  <p className="text-sm text-green-800">{successMessage}</p>
                </div>
              )}

              <UserForm onSubmit={handleSubmit} onCancel={handleCancel} />
            </div>

            <div className="bg-gray-50 px-6 py-4 border-t border-gray-200">
              <div className="flex items-center justify-between">
                <div className="text-sm text-gray-600">
                  <span className="font-semibold">PF3</span> = Return to Admin Menu | <span className="font-semibold">PF4</span> = Clear Screen
                </div>
                <Button variant="secondary" onClick={handlePF3}>PF3 - Admin Menu</Button>
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}
