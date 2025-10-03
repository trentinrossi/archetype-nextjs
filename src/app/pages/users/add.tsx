'use client';

import { useRouter } from 'next/navigation';
import Link from 'next/link';
import UserForm from '@/components/UserForm';
import { CreateUserRequest } from '@/types/user';

interface BreadcrumbItem {
  label: string;
  href?: string;
}

export default function AddUserPage() {
  const router = useRouter();

  const breadcrumbs: BreadcrumbItem[] = [
    { label: 'Home', href: '/' },
    { label: 'User Management', href: '/users' },
    { label: 'Add User' },
  ];

  const handleSave = async (userData: CreateUserRequest) => {
    // UserForm component handles the API call
    // After successful save, stay on the form for more entries
  };

  const handleSaveAndExit = async (userData: CreateUserRequest) => {
    // UserForm component handles the API call
    // After successful save, redirect to users list
    router.push('/users');
  };

  const handleCancel = () => {
    router.push('/users');
  };

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="py-4">
            {/* Breadcrumbs */}
            <nav className="flex mb-4" aria-label="Breadcrumb">
              <ol className="flex items-center space-x-2">
                {breadcrumbs.map((item, index) => (
                  <li key={index} className="flex items-center">
                    {index > 0 && (
                      <span className="mx-2 text-gray-400">/</span>
                    )}
                    {item.href ? (
                      <Link
                        href={item.href}
                        className="text-blue-600 hover:text-blue-800 text-sm font-medium"
                      >
                        {item.label}
                      </Link>
                    ) : (
                      <span className="text-gray-900 text-sm font-medium">
                        {item.label}
                      </span>
                    )}
                  </li>
                ))}
              </ol>
            </nav>

            {/* Page Title */}
            <div>
              <h1 className="text-3xl font-bold text-gray-900">
                Add New User
              </h1>
              <p className="text-gray-600 mt-1">
                Create a new user account with required information
              </p>
            </div>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="bg-white rounded-lg shadow-sm border">
          <UserForm
            onSave={handleSave}
            onSaveAndExit={handleSaveAndExit}
            onCancel={handleCancel}
          />
        </div>
      </div>
    </div>
  );
}