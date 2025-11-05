'use client';

import React, { useEffect, useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { userService } from '@/services/userService';
import { User } from '@/types/user';

export default function UserDeletePage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const [currentDateTime, setCurrentDateTime] = useState(new Date());
  const [currentUser, setCurrentUser] = useState<any>(null);
  const [selectedUser, setSelectedUser] = useState<User | null>(null);
  const [userId, setUserId] = useState('');
  const [loading, setLoading] = useState(false);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);

  useEffect(() => {
    const user = userService.getCurrentUser();
    if (!user || user.userType !== 'A') {
      router.push('/login');
      return;
    }
    setCurrentUser(user);

    const userIdParam = searchParams.get('userId');
    if (userIdParam) {
      setUserId(userIdParam);
      fetchUser(userIdParam);
    }

    const timer = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);

    return () => clearInterval(timer);
  }, [router, searchParams]);

  const formatDateTime = (date: Date): string => {
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const year = date.getFullYear();
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    const seconds = String(date.getSeconds()).padStart(2, '0');
    return `${month}/${day}/${year} ${hours}:${minutes}:${seconds}`;
  };

  const fetchUser = async (id: string) => {
    try {
      setLoading(true);
      const user = await userService.getUserById(id);
      setSelectedUser(user);
    } catch (err: any) {
      alert(err.message || 'User not found');
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = (e: React.FormEvent) => {
    e.preventDefault();
    if (userId.trim()) {
      fetchUser(userId.trim());
    }
  };

  const handleDelete = async () => {
    if (!selectedUser) return;

    if (!confirm(`Are you sure you want to delete user ${selectedUser.userId}?`)) {
      return;
    }

    try {
      const response = await userService.deleteUser(selectedUser.userId);
      setSuccessMessage(response.message);
      setTimeout(() => {
        router.push('/admin/users');
      }, 2000);
    } catch (err: any) {
      alert(err.message || 'Failed to delete user');
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
              <div className="flex items-center justify-center w-10 h-10 bg-red-600 rounded-lg mr-3">
                <svg className="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16" />
                </svg>
              </div>
              <div>
                <h1 className="text-2xl font-bold text-gray-900">Delete User</h1>
                <p className="text-xs text-gray-600">Transaction: CU03 | Program: COUSR03C</p>
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
            <div className="bg-gradient-to-r from-red-600 to-red-700 px-6 py-4">
              <h2 className="text-xl font-bold text-white">Delete User</h2>
              <p className="text-red-100 text-sm mt-1">Search for user to delete</p>
            </div>

            <div className="p-6">
              {!selectedUser && (
                <form onSubmit={handleSearch} className="mb-6">
                  <div className="flex gap-4 items-end">
                    <div className="flex-1">
                      <Input
                        label="User ID"
                        value={userId}
                        onChange={(e) => setUserId(e.target.value)}
                        placeholder="Enter User ID"
                        maxLength={8}
                        required
                      />
                    </div>
                    <Button type="submit" disabled={loading}>
                      {loading ? 'Searching...' : 'Search'}
                    </Button>
                  </div>
                </form>
              )}

              {successMessage && (
                <div className="mb-6 bg-green-50 border border-green-200 rounded-lg p-4">
                  <p className="text-sm text-green-800">{successMessage}</p>
                </div>
              )}

              {selectedUser && (
                <div>
                  <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4 mb-6">
                    <div className="flex">
                      <div className="flex-shrink-0">
                        <svg className="h-5 w-5 text-yellow-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                          <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                        </svg>
                      </div>
                      <div className="ml-3">
                        <h3 className="text-sm font-medium text-yellow-800">Warning</h3>
                        <div className="mt-2 text-sm text-yellow-700">
                          <p>You are about to delete this user. This action cannot be undone.</p>
                        </div>
                      </div>
                    </div>
                  </div>

                  <div className="bg-gray-50 rounded-lg p-6 mb-6">
                    <h3 className="text-lg font-semibold text-gray-900 mb-4">User Information</h3>
                    <dl className="grid grid-cols-1 gap-4">
                      <div>
                        <dt className="text-sm font-medium text-gray-500">User ID</dt>
                        <dd className="mt-1 text-sm text-gray-900">{selectedUser.userId}</dd>
                      </div>
                      <div>
                        <dt className="text-sm font-medium text-gray-500">First Name</dt>
                        <dd className="mt-1 text-sm text-gray-900">{selectedUser.firstName}</dd>
                      </div>
                      <div>
                        <dt className="text-sm font-medium text-gray-500">Last Name</dt>
                        <dd className="mt-1 text-sm text-gray-900">{selectedUser.lastName}</dd>
                      </div>
                      <div>
                        <dt className="text-sm font-medium text-gray-500">User Type</dt>
                        <dd className="mt-1 text-sm text-gray-900">{selectedUser.userType === 'A' ? 'Admin' : 'Regular'}</dd>
                      </div>
                    </dl>
                  </div>

                  <div className="flex gap-2">
                    <Button variant="danger" onClick={handleDelete}>
                      PF5 - Delete User
                    </Button>
                    <Button variant="secondary" onClick={handleCancel}>
                      Cancel
                    </Button>
                  </div>
                </div>
              )}
            </div>

            <div className="bg-gray-50 px-6 py-4 border-t border-gray-200">
              <div className="flex items-center justify-between">
                <div className="text-sm text-gray-600">
                  <span className="font-semibold">PF3</span> = Return to Admin Menu | <span className="font-semibold">PF5</span> = Delete User
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
