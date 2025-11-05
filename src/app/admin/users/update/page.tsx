'use client';

import React, { useEffect, useState } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { userService } from '@/services/userService';
import UserForm from '@/components/UserForm';
import { UpdateUserRequest, User } from '@/types/user';

export default function UserUpdatePage() {
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

  const handleSubmit = async (data: UpdateUserRequest) => {
    if (!selectedUser) return;

    try {
      const response = await userService.updateUser(selectedUser.userId, data);
      setSuccessMessage(response.message);
      setTimeout(() => {
        router.push('/admin/users');
      }, 2000);
    } catch (err: any) {
      alert(err.message || 'Failed to update user');
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
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z" />
                </svg>
              </div>
              <div>
                <h1 className="text-2xl font-bold text-gray-900">Update User</h1>
                <p className="text-xs text-gray-600">Transaction: CU02 | Program: COUSR02C</p>
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
              <h2 className="text-xl font-bold text-white">Update User Information</h2>
              <p className="text-indigo-100 text-sm mt-1">Search for user and modify information</p>
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
                <UserForm
                  onSubmit={handleSubmit}
                  onCancel={handleCancel}
                  initialData={selectedUser}
                />
              )}
            </div>

            <div className="bg-gray-50 px-6 py-4 border-t border-gray-200">
              <div className="flex items-center justify-between">
                <div className="text-sm text-gray-600">
                  <span className="font-semibold">PF3</span> = Return to Admin Menu | <span className="font-semibold">PF5</span> = Save Changes
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
